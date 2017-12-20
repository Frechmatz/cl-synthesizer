(in-package :cl-synthesizer-examples)

(defun event-aggregator ()
  (declare (optimize (debug 3) (speed 0) (space 0)))
  ;; events: { eventId, moduleName, eventName, count  }*
  (let ((events nil))
    (list
     :inc
     (lambda (event-id module-name event-name)
       (let ((l (find-if (lambda (i) (eq event-id (first i))) events)))
	 (if (not l)
	     (progn
	       (setf l (list event-id module-name event-name 0))
	       (push l events)))
	 (let ((c (fourth l)))
	   (setf (fourth l) (if c (+ c 1) 1)))))
     :reset (lambda()
	      (dolist (e events)
		(setf (fourth e) nil)))
     :get-events (lambda () events))))

(defun console-logger (rack)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let* ((sample-rate (getf (slot-value rack 'cl-synthesizer::environment) :sample-rate))
	 (every-ticks (/ sample-rate 5))
	 (cur-tick 0)
	 (total-ticks 0)
	 (aggregator (event-aggregator)))
    (lambda (event-id module-name event-name)
      (declare (optimize (debug 3) (speed 0) (space 0)))
      (setf total-ticks (+ 1 total-ticks))
      (setf cur-tick (+ 1 cur-tick))
      (funcall (getf aggregator :inc) event-id module-name event-name)
      (if (>= cur-tick every-ticks)
	  (progn
	    (dolist (event (funcall (getf aggregator :get-events)))
	      (if (fourth event)
		  (format t "~a: ~a (~a) tickTimestamp: ~a~%"
			  (second event)
			  (third event)
			  (fourth event)
			  total-ticks))
	      (setf (fourth event) nil))
	    (funcall (getf aggregator :reset))
	    (setf cur-tick 0))))))


(defun play-rack (rack duration-seconds)
  (let ((start (get-internal-real-time))
	(ticks-to-play (* duration-seconds (getf (slot-value rack 'cl-synthesizer::environment) :sample-rate)))
	(console-logger (console-logger rack)))
    (format t "~%Ticks to play: ~a~%" ticks-to-play)
    (cl-synthesizer::add-event-listener
     rack
     "Console-Logger"
     (lambda (event-id module-name event-name)
       (funcall console-logger event-id module-name event-name)))
    (dotimes (i ticks-to-play)
      (cl-synthesizer::update-rack rack))
    (cl-synthesizer::shutdown-rack rack)
    (let ((end (get-internal-real-time)))
      (format t "~%Elapsed time in seconds after shutdown: ~a~%" (/ (- end start) internal-time-units-per-second))))
  "DONE")

