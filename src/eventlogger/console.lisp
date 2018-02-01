(in-package :cl-synthesizer-event-logger)

(defun console (environment)
  (let* ((sample-rate (getf environment :sample-rate))
	 (every-ticks (/ sample-rate 5))
	 (cur-tick 0)
	 (total-ticks 0)
	 (aggregator (event-aggregator))
	 (add-event (getf aggregator :add-event)))
    (labels ((flush ()
	       (with-aggregated-events aggregator event-id event-count
		 (format t "~a: ~a tickTimestamp: ~a~%" event-id event-count total-ticks))
	       (funcall (getf aggregator :reset))
	       (setf cur-tick 0)))
      (list
       :log
       (lambda (event-id)
	 (funcall add-event event-id))
       :tick
       (lambda ()
	 (setf total-ticks (+ 1 total-ticks))
	 (setf cur-tick (+ 1 cur-tick))
	 (if (>= cur-tick every-ticks)
	     (flush)))
      :flush #'flush))))
