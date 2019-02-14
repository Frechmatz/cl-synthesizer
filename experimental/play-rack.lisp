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
  (flet ((make-audio-output-getter ()
	   (let ((cur-channel 0)
		 (get-output (getf rack :get-output))
		 (audio-input-arguments nil) ;; Shared list for all audio updates
		 (audio-input-lambdas nil))
	     (dolist (socket audio-output-sockets)
	       (let ((cur-socket socket))
		 (if (not (find cur-socket (funcall (getf rack :outputs))))
		     (cl-synthesizer:signal-assembly-error
		      :format-control "Audio output socket ~a not exposed by rack"
		      :format-arguments (list cur-socket)))
		 (let ((device-socket (cl-synthesizer-macro-util:make-keyword "channel" cur-channel)))
		   ;; Push updater function (modifies audio-input-arguments)
		   (push (lambda ()
			   (setf (getf audio-input-arguments device-socket)
				 (funcall get-output cur-socket)))
			 audio-input-lambdas)
		   ;; Value
		   (push nil audio-input-arguments)
		   ;; Key
		   (push device-socket audio-input-arguments)))
	       (setf cur-channel (+ 1 cur-channel)))
	     (lambda ()
	       (dolist (fn audio-input-lambdas)
		 (funcall fn))
	       audio-input-arguments))))
    (if (or (not attach-audio) (eq 0 (length audio-output-sockets)))
	(values
	 (lambda () nil)
	 (lambda () nil))
	(let ((device
	       (make-device
		"SPEAKER" environment
		(list :channel-count (length audio-output-sockets))
		*audio-device-settings*))
	      (getter (make-audio-output-getter)))
	  (values
	   (lambda () (funcall (getf device :update) (funcall getter)))
	   (lambda () (funcall (getf device :shutdown))))))))

(defun prepare-midi-input (rack environment attach-midi midi-input-socket)
  (declare (ignore rack))
  (flet ((make-midi-input-getter (device)
	   (let ((midi-output nil) ;; Shared list for all device requests
		 (update-device (getf device :update))
		 (get-device-output (getf device :get-output)))
	     ;; Value
	     (push nil midi-output)
	     ;; Key
	     (push midi-input-socket midi-output)
	     (lambda ()
	       (funcall update-device) ;; update MIDI device
	       (setf (getf midi-output midi-input-socket) (funcall get-device-output nil))
	       midi-output))))
    (if (or (not attach-midi) (not midi-input-socket))
	(values
	 (lambda () nil)
	 (lambda () nil))
	(let* ((device (make-device "MIDI" environment nil *midi-device-settings*))
	       (getter (make-midi-input-getter device)))
	  (values
	   (lambda () (funcall getter))
	   (lambda () (funcall (getf device :shutdown))))))))
	

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
  (let ((environment (getf rack :environment)) (f (getf rack :update)))
    (multiple-value-bind (update-audio shutdown-audio)
	(prepare-audio-output rack environment attach-audio audio-output-sockets)
      (multiple-value-bind (get-midi-input shutdown-midi)
	  (prepare-midi-input rack environment attach-midi midi-input-socket)
	(dotimes (i (* duration-seconds (floor (getf environment :sample-rate))))
	  (funcall f (funcall get-midi-input))
	  (funcall update-audio))
	(funcall (getf rack :shutdown))
	(funcall shutdown-audio)
	(funcall shutdown-midi)
	"DONE"))))
  