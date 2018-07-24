(in-package :cl-synthesizer-util)

(defun play-rack (rack duration-seconds &key (attach-speaker nil) (attach-midi nil) (midi-device nil))
  (let* ((start (get-internal-real-time))
	 (environment (slot-value rack 'cl-synthesizer::environment))
	 (sample-rate (getf environment :sample-rate))
	 (ticks-to-play (* duration-seconds sample-rate)))
    (format t "~%Ticks to play: ~a~%" ticks-to-play)
    (if attach-speaker
	(funcall (getf (cl-synthesizer:get-line-out-adapter rack) :set-device)
		 (cl-synthesizer-device-speaker:speaker-cl-out123 "SPEAKER" environment :channel-count 2 :driver "coreaudio")))
    (if midi-device
	(funcall (getf (cl-synthesizer:get-midi-in-adapter rack) :set-device) midi-device)
	(if attach-midi
	    (funcall (getf (cl-synthesizer:get-midi-in-adapter rack) :set-device)
		     (cl-synthesizer-device-midi:midi-device "MIDI" environment))))
    (dotimes (i ticks-to-play)
      (cl-synthesizer:update rack))
    (cl-synthesizer:shutdown rack)
    (let ((end (get-internal-real-time)))
      (format t "~%Elapsed time in seconds after shutdown: ~a~%"
	      (/ (- end start) internal-time-units-per-second))))
  "DONE")

