(in-package :cl-synthesizer-midi)

(defun relative-cc-handler (midi-controller controllers &key (cv-initial 2.5) (cv-min 0) (cv-max 5) (channel nil))
  "Returns a handler that converts relative MIDI CC-Events to control voltages. The handler
   consists of a property list with the following properties:
   - :update Update function to be called with a list of midi-events
   - :get-output A function that returns the current control voltage.
   The handler can work with multiple encoders, where each encoder has a \"weight\" which
   defines how strongly the control voltage will be changed.
   Arguments:
   - midi-controller: A property list with the following properties:
     -- :get-controller-number: A function that is called with a keyword that identifies 
         the controller, for example :ENCODER-1 and returns the controller number, 
         for example 112.
     -- :get-controller-value-offset A function that is called with the value of a 
         CC event, for example 62, and returns a positive or negative offset, for example -3.
   - controllers: (list (:controller-id <id> :delta-percent <delta-percent> :turn-speed <speed>)) 
     -- <controller-id>: A keyword that identifies an encoder of the midi-controller, for example :encoder-1
     -- <delta-percent>: Defines how much the control-voltage will be increased/decreased when the controller
        is turned. The value is relative to the total control voltage range as defined by cv-min and cv-max.
     -- <speed>: A lambda that is called with the absolute value of the increase/decrease offset as returned by
        the :get-controller-value-offset function of the midi-controller. The offset typically depends on the 
        speed with which the encoder is turned. The lambda must return the absolute value of the new offset. 
        Can for example be used to disable turn-speed specific increments/decrements by returning 1. 
        Example: (:turn-speed (lambda (offs) 1))
    Input example 1: '((:controller-id :ENCODER-1 :delta-percent 0.01) (:controller-id :ENCODER-2 :delta-percent 0.10))
    Input example 2: '((:controller-id :ENCODER-1 :delta-percent 0.01 :turn-speed (lambda(offs) 1)))"
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (if (< cv-max cv-min)
      (cl-synthesizer:signal-assembly-error
       :format-control "cv-min greater than cv-max ~a ~a"
       :format-arguments (list cv-min cv-max)))
  (if (or (< cv-max cv-initial) (> cv-min cv-initial))
      (cl-synthesizer:signal-assembly-error
       :format-control "Initial value ~a not in CV range ~a - ~a"
       :format-arguments (list cv-initial cv-min cv-max)))
  (let ((controller-handlers nil) (cur-cv cv-initial) (cv-range (abs (- cv-max cv-min))))
    (dolist (controller controllers)
      (let ((ctrl-id (funcall (getf midi-controller :get-controller-number) (getf controller :controller-id)))
	    (delta (* cv-range (getf controller :delta-percent)))
	    (turn-speed (if (getf controller :turn-speed) (getf controller :turn-speed) (lambda (offs) offs))))
	(push
	 (lambda (midi-event-controller-id midi-event-controller-offset)
	   (if (= midi-event-controller-id ctrl-id)
	       (let ((speed
		      (*
		       (signum midi-event-controller-offset)
		       (funcall turn-speed (abs midi-event-controller-offset)))))
		 (* delta speed))
	       0))
	 controller-handlers)))
    (list
     :update (lambda (midi-events)
	       (let ((delta 0))
		 (dolist (midi-event midi-events)
		   (if (and
			(cl-synthesizer-midi-event:control-change-eventp midi-event)
			(or (not channel)
			    (= channel (cl-synthesizer-midi-event:get-channel midi-event))))
		       (let ((controller-number (cl-synthesizer-midi-event:get-controller-number midi-event))
			     (offset
			      (funcall
			       (getf midi-controller :get-controller-value-offset)
			       (cl-synthesizer-midi-event:get-controller-value midi-event))))
			 (dolist (controller-handler controller-handlers)
			   (setf delta (+ delta (funcall controller-handler controller-number offset)))))))
		 (let ((tmp (+ cur-cv delta)))
		   ;; apply clipping
		   (if (> cv-min tmp)
		       (setf cur-cv cv-min)
		       (if (< cv-max tmp)
			   (setf cur-cv cv-max)
			   (setf cur-cv tmp))))))
     :get-output (lambda () cur-cv))))
