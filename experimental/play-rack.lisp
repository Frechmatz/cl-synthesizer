(in-package :cl-synthesizer-experimental)

(defvar *audio-device-settings*
  (list
   :symbol-name "SPEAKER-CL-OUT123"
   :package-name "CL-SYNTHESIZER-DEVICE-SPEAKER"
   :init-args
   (list
    :channel-count (lambda (context) (getf context :channel-count))
    :driver "coreaudio")))

(defvar *midi-device-settings*
  (list
   :symbol-name "MIDI-DEVICE"
   :package-name "CL-SYNTHESIZER-DEVICE-MIDI"
   :init-args (list :source-index 1)))

(defun make-device (name environment device-context device-settings)
  (flet ((prepare-init-args ()
	   (let ((prepared nil) (initargs (getf device-settings :init-args)))
	     (dotimes (i (length initargs))
	       (let ((item (nth i initargs)))
		 (if (functionp item)
		     (push (funcall item device-context) prepared)
		     (push item prepared))))
	     (reverse prepared))))
    (let ((symbol-name (getf device-settings :symbol-name))
	  (package-name (getf device-settings :package-name)))
      (if (not symbol-name)
	  (error "Device instantiation failed. :symbol-name is a mandatory device configuration property")) 
      (if (not package-name)
	  (error "Device instantiation failed. :package-name is a mandatory device configuration property")) 
      (format t "~%Instantiating device ~a::~a..." symbol-name package-name)
      (let ((ctor (find-symbol symbol-name package-name)))
	(if ctor
	    (apply ctor name environment (prepare-init-args))
	    (error (format nil "Device instantiation failed. Symbol ~a::~a not found" symbol-name package-name)))))))

(defun prepare-audio-output (rack environment attach-audio audio-output-sockets)
  "Returns a values object consisting of two lambdas:
  - Fetch audio-outputs from rack, set inputs of speaker, update speaker
  - Speaker shutdown function"
  (if (or (not attach-audio) (eq 0 (length audio-output-sockets)))
      (values (lambda() nil) (lambda() nil))
      (let* ((speaker
	     ;; Instantiate device
	     (make-device
	      "SPEAKER" environment
	      (list :channel-count (length audio-output-sockets))
	      *audio-device-settings*))
	     (rack-outputs (funcall (getf rack :outputs)))
	     (speaker-inputs (funcall (getf speaker :inputs)))
	     (speaker-update-fn (getf speaker :update))
	     (speaker-input-setters nil))
	(dotimes (index (length audio-output-sockets))
	  (let ((cur-audio-output-socket (nth index audio-output-sockets))
		(cur-speaker-input-socket (cl-synthesizer-macro-util:make-keyword "channel" index)))
	    (let ((output-getter (getf rack-outputs cur-audio-output-socket))
		  (input-setter (getf speaker-inputs cur-speaker-input-socket)))
	      
	      (if (not output-getter)
		  (cl-synthesizer:signal-assembly-error
		   :format-control "Audio output socket ~a not exposed by rack"
		   :format-arguments (list cur-audio-output-socket)))
	      
	      (if (not input-setter)
		  (cl-synthesizer:signal-assembly-error
		   :format-control "Internal error. Could not find setter for socket ~a"
		   :format-arguments (list cur-speaker-input-socket)))
	      
	      (push (lambda () (funcall input-setter (funcall output-getter))) speaker-input-setters))))

	;; Compile
	(let ((speaker-update-lambda
	       (lambda()
		 ;; Set inputs of speaker
		 (dolist (fn speaker-input-setters)
		   (funcall fn))
		 ;; Update speaker
		 (funcall speaker-update-fn))))
	  (values
	   speaker-update-lambda
	   (lambda () (funcall (getf speaker :shutdown))))))))

(defun prepare-midi-input (rack environment attach-midi midi-input-socket)
  "Returns a values object consisting of two lambdas:
  - Update Midi-Device, fetch midi output, set midi-input of rack
  - Midi shutdown function"
  (if (or (not attach-midi) (not midi-input-socket))
      (values
       (lambda () nil)
       (lambda () nil))
      (let* ((midi-device
	      ;; Instantiate device
	      (make-device "MIDI" environment nil *midi-device-settings*))
	     (midi-output-getter (getf (funcall (getf midi-device :outputs)) :midi-events))
	     (midi-update-fn (getf midi-device :update))
	     (rack-input-setter (getf (funcall (getf rack :inputs)) midi-input-socket)))
	
	(if (not rack-input-setter)
	    (cl-synthesizer:signal-assembly-error
	     :format-control "Midi input socket ~a not exposed by rack"
	     :format-arguments (list midi-input-socket)))

	(if (not midi-output-getter)
	    (cl-synthesizer:signal-assembly-error
	     :format-control "Internal error: Midi device does not expose output :midi-events"
	     :format-arguments nil))

	(let ((midi-update-lambda
	       (lambda()
		 ;; Update Midi device
		 (funcall midi-update-fn)
		 ;; Push Midi output into rack
		 (funcall rack-input-setter (funcall midi-output-getter)))))
	  
	  (values
	   midi-update-lambda
	   (lambda () (funcall (getf midi-device :shutdown))))))))


(defun play-rack (rack &key duration-seconds  (attach-audio nil) (audio-output-sockets nil)
					    (attach-midi nil) (midi-input-socket nil))
  "A utility function that \"plays\" the rack by consecutively calling its update function
    for a given number of \"ticks\". The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
	<li>duration-seconds Duration in seconds of how long to play the rack. If for
	    example the duration is 2 seconds and the sample rate of the rack as declared
	    by its environment is 44100, then the update function of the rack will be called 88200 times.</li>
	<li>:attach-audio If t then the audio device as declared by the variable *audio-device-settings* 
            is instantiated and attached to the given outputs of the rack.</li>
        <li>:audio-output-sockets A list of keywords that declare the output sockets of the rack
            providing the audio signal.</li>
	<li>:attach-midi If t then the MIDI device as declared by the variable *midi-device-settings* 
            is instantiated and attached to the rack.</li>
        <li>:midi-input-socket A keyword that declares the input socket of the rack
            to which the MIDI input is to be routed.</li>
    </ul>
    The current implementation of the play-rack function assumes that an audio device is blocking.
    <p>See also: cl-synthesizer-device-speaker:speaker-cl-out123, cl-synthesizer-device-midi:midi-device</p>"
  (let ((environment (getf rack :environment)) (rack-update-fn (getf rack :update)))
    (multiple-value-bind (update-audio shutdown-audio)
	(prepare-audio-output rack environment attach-audio audio-output-sockets)
      (multiple-value-bind (push-midi-input shutdown-midi)
	  (prepare-midi-input rack environment attach-midi midi-input-socket)
	(dotimes (i (* duration-seconds (floor (getf environment :sample-rate))))
	  ;; Push events of Midi device into rack
	  (funcall push-midi-input)
	  ;; Update rack
	  (funcall rack-update-fn)
	  ;; Push audio outputs of rack into audio device
	  (funcall update-audio))
	(funcall (getf rack :shutdown))
	(funcall shutdown-audio)
	(funcall shutdown-midi)
	"DONE"))))
  
