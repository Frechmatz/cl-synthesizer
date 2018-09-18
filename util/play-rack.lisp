(in-package :cl-synthesizer-util)

(defun prepare-init-args (environment initargs)
  "Resolves init arguments that are defined as functions by calling the 
  corresponding function with the environment as argument."
  (let ((prepared nil))
    (dotimes (i (length initargs))
      (let ((item (nth i initargs)))
	(if (functionp item)
	    (push (funcall item environment) prepared)
	    (push item prepared))))
    (reverse prepared)))

(defun instantiate-device (device-settings name environment)
  (let ((symbol-name (getf device-settings :symbol-name))
	(package-name (getf device-settings :package-name)))
    (if (not symbol-name)
	(error "Device instantiation failed. :symbol-name is a mandatory device configuration property")) 
    (if (not package-name)
	(error "Device instantiation failed. :package-name is a mandatory device configuration property")) 
    (format t "~%Instantiating device ~a::~a..." symbol-name package-name)
    (let ((ctor (find-symbol symbol-name package-name)))
      (if ctor
	  (apply ctor name environment (prepare-init-args environment (getf device-settings :init-args)))
	  (error (format nil "Device instantiation failed. Symbol ~a::~a not found" symbol-name package-name))))))

(defun make-audio-device (name environment)
  (let ((settings (getf environment :audio-device)))
    (if settings
	(instantiate-device
	 settings
	 name
	 environment)
	(error (format nil "Audio device requested but not configured by environment")))))

(defun make-midi-device (name environment)
  (let ((settings (getf environment :midi-device)))
    (if settings
	(instantiate-device
	 settings
	 name
	 environment)
	(error (format nil "MIDI device requested but not configured by environment")))))

(defun play-rack (rack duration-seconds &key (attach-speaker nil) (attach-midi nil))
  (let* ((environment (slot-value rack 'cl-synthesizer::environment)))
    (if attach-speaker
	(funcall (getf (cl-synthesizer:get-line-out-adapter rack) :set-device)
		 (make-audio-device "SPEAKER" environment)))
    (if attach-midi
	(funcall (getf (cl-synthesizer:get-midi-in-adapter rack) :set-device)
		 (make-midi-device "MIDI" environment)))
    (dotimes (i (* duration-seconds (getf environment :sample-rate)))
      (cl-synthesizer:update rack))
    (cl-synthesizer:shutdown rack)
    "DONE"))

