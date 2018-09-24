(in-package :cl-synthesizer)

(defun make-audio-device (name environment)
  (let ((settings (getf environment :audio-device)))
    (if settings
	(make-device
	 name
	 environment
	 settings)
	(error (format nil "Audio device requested but not configured by environment")))))

(defun make-midi-device (name environment)
  (let ((settings (getf environment :midi-device)))
    (if settings
	(make-device
	 name
	 environment
	 settings)
	(error (format nil "MIDI device requested but not configured by environment")))))

(defun play-rack (rack duration-seconds &key (attach-speaker nil) (attach-midi nil))
  (let* ((environment (slot-value rack 'environment)))
    (if attach-speaker
	(funcall (getf (get-line-out-adapter rack) :set-device)
		 (make-audio-device "SPEAKER" environment)))
    (if attach-midi
	(funcall (getf (get-midi-in-adapter rack) :set-device)
		 (make-midi-device "MIDI" environment)))
    (dotimes (i (* duration-seconds (getf environment :sample-rate)))
      (update rack))
    (shutdown rack)
    "DONE"))

