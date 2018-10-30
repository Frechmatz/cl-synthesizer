(in-package :cl-synthesizer)

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

(defun make-audio-getter (rack sockets)
  (let ((map nil) (cur-channel 0) (get-output (getf rack :get-output)))
    (dolist (socket sockets)
      (let ((cur-socket socket))
	(push (lambda () (funcall get-output cur-socket)) map)
	(push (cl-synthesizer-macro-util:make-keyword "channel" cur-channel) map)
	(setf cur-channel (+ 1 cur-channel))))
    (lambda ()
      (mapcar (lambda(item) (if (keywordp item) item (funcall item))) map))))

(defun make-midi-getter (device socket)
  (let ((update (getf device :update)) (get-output (getf device :get-output)))
    (lambda ()
      (funcall update)
      (list socket (funcall get-output nil)))))

(defun play-rack (rack duration-seconds &key (attach-audio nil) (audio-output-sockets nil)
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
        <li>:midi-input-socket A keyword that declares the input socket of the rack to
            to which the MIDI input is to be routed.</li>
    </ul>
    The current implementation of the play-rack function assumes that an audio device is blocking.
    <p>See also: cl-synthesizer-device-speaker:speaker-cl-out123</p>
    <p>See also: cl-synthesizer-device-midi:midi-device</p>"
  (let* ((environment (getf rack :environment))
	 (audio-device nil) (audio-getter nil)
	 (midi-device nil) (midi-getter nil))
    (if (and attach-audio audio-output-sockets)
	(progn
	  (setf audio-device (make-device "SPEAKER" environment
					  (list :channel-count (length audio-output-sockets))
					  *audio-device-settings*))
	  (setf audio-getter (make-audio-getter rack audio-output-sockets))))
    (if (and attach-midi midi-input-socket)
	(progn
	  (setf midi-device (make-device "MIDI" environment nil *midi-device-settings*))
	  (setf midi-getter (make-midi-getter midi-device midi-input-socket))))
    (let ((f (getf rack :update)))
      (dotimes (i (* duration-seconds (getf environment :sample-rate)))
	(let ((input (if midi-device (funcall midi-getter) nil)))
	  (apply f input)
	  (if audio-device
	      (apply (getf audio-device :update) (funcall audio-getter))))))
    (funcall (getf rack :shutdown))
    (if audio-device
	(funcall (getf audio-device :shutdown)))
    (if midi-device
	(funcall (getf midi-device :shutdown)))
    "DONE"))

