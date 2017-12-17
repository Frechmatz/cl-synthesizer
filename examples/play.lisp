(in-package :cl-synthesizer-examples)



(defun console-logger (rack)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let* ((sample-rate (getf (slot-value rack 'cl-synthesizer::environment) :sample-rate))
	 (every-ticks (/ sample-rate 5))
	 (cur-tick 0)
	 (total-ticks 0)
	 (start-time (get-internal-real-time)))
    (lambda ()
      (declare (optimize (debug 3) (speed 0) (space 0)))
      (setf total-ticks (+ 1 total-ticks))
      (setf cur-tick (+ 1 cur-tick))
      (if (>= cur-tick every-ticks)
	  (let ((deltaT (- (get-internal-real-time) start-time)))
	    (dolist (event (cl-synthesizer::get-events rack))
	      (format t "~a: ~a (~a) ticks: ~a~%"
		      (first event)
		      (second event)
		      (third event)
		      total-ticks
		      ))
	    (setf cur-tick 0))))))

(defun play-rack (rack duration-seconds)
  (let ((start (get-internal-real-time))
	(logger (console-logger rack))
	(ticks-to-play (* duration-seconds (getf (slot-value rack 'cl-synthesizer::environment) :sample-rate))))
    (format t "~%Ticks to play: ~a~%" ticks-to-play)
    (dotimes (i ticks-to-play)
      (cl-synthesizer::update-rack rack)
      (funcall logger))
    (cl-synthesizer::shutdown-rack rack)
    (let ((end (get-internal-real-time)))
      (format t "~%Elapsed time in seconds after shutdown: ~a~%" (/ (- end start) internal-time-units-per-second))))
  "DONE")

