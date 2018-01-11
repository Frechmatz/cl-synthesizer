(in-package :cl-synthesizer-examples)

(defun event-aggregator ()
  (declare (optimize (debug 3) (speed 0) (space 0)))
  ;; events: ( (eventId, count, moduleName, eventName) .... )
  (let ((events nil))
    (list
     :add-event
     (lambda (event-id module-name event-name)
       (let ((l (find-if (lambda (i) (eq event-id (first i))) events)))
	 (if (not l)
	     (progn
	       (setf l (list event-id 0 module-name event-name))
	       (push l events)))
	 (let ((c (second l)))
	   (setf (second l) (if c (+ c 1) 1)))))
     :reset (lambda()
	      (dolist (e events)
		(setf (second e) nil)))
     :get-events (lambda () events))))

(defmacro with-aggregator-events (event-aggregator event-id module-name event-name count &body body)
  (let ((event (gensym)))
    `(dolist (,event (funcall (getf ,event-aggregator :get-events)))
       (if (second ,event)
	   (let ((,event-id (first ,event))
		 (,module-name (third ,event))
		 (,event-name (fourth ,event))
		 (,count (second ,event)))
	     ,@body)))))

(defun console-logger (rack)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let* ((sample-rate (getf (slot-value rack 'cl-synthesizer::environment) :sample-rate))
	 (every-ticks (/ sample-rate 5))
	 (cur-tick 0)
	 (total-ticks 0)
	 (aggregator (event-aggregator)))
    (labels ((log-event (event-id module-name event-name)
	       (declare (optimize (debug 3) (speed 0) (space 0)))
	       (funcall (getf aggregator :add-event) event-id module-name event-name))
	     (flush ()
	       (with-aggregator-events
		   aggregator event-id module-name event-name event-count
		 (declare (ignore event-id))
		 (format t "~a: ~a (~a) tickTimestamp: ~a~%" module-name event-name event-count total-ticks))
	       (funcall (getf aggregator :reset))
	       (setf cur-tick 0))
	     (tick ()
	       (declare (optimize (debug 3) (speed 0) (space 0)))
	       (setf total-ticks (+ 1 total-ticks))
	       (setf cur-tick (+ 1 cur-tick))
	       (if (>= cur-tick every-ticks)
		   (flush))))
      (list
       :log #'log-event
       :tick #'tick
       :flush #'flush))))
  
(defun play-rack-impl (rack duration-seconds attach-speaker)
  (let* ((start (get-internal-real-time))
	 (environment (slot-value rack 'cl-synthesizer::environment))
	 (sample-rate (getf environment :sample-rate))
	 (ticks-to-play (* duration-seconds sample-rate))
	 (console-logger (console-logger rack)))
    (format t "~%Ticks to play: ~a~%" ticks-to-play)
    ;; Add event listener
    (cl-synthesizer::add-event-listener
     rack
     "Console-Logger"
     (getf console-logger :log)
     :tick-fn (getf console-logger :tick)
     :shutdown-fn (getf console-logger :flush))
    ;; Set audio device
    (if attach-speaker
	(funcall (getf (cl-synthesizer:get-line-out rack) :set-device)
		 (cl-synthesizer-device-speaker:stereo-speaker environment :driver "coreaudio")))
    (dotimes (i ticks-to-play)
      (cl-synthesizer::update-rack rack))
    (cl-synthesizer::shutdown-rack rack)
    (let ((end (get-internal-real-time)))
      (format t "~%Elapsed time in seconds after shutdown: ~a~%" (/ (- end start) internal-time-units-per-second))))
  "DONE")

(defun play-rack (rack duration-seconds)
  (play-rack-impl rack duration-seconds nil))

(defun play-rack-with-audio-output (rack duration-seconds)
  (play-rack-impl rack duration-seconds t))

