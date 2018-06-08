(in-package :cl-synthesizer-midi)

(defun relative-cc-handler (midi-controller inputs &key (cv-initial 2.5) (cv-min 0) (cv-max 5))
  "Returns a handler that converts relative MIDI CC-Events to control voltages. The handler
   consists of a property list with the following properties:
   - :update Update function to be called with a list of midi-events
   - :get-output A function that returns the current control voltage.
   The handler can work with multiple encoders, where each encoder has a \"weight\" which
   defines how strongly the control voltage will be changed.
   Arguments:
   - midi-controller: The MIDI controller. Defines keywords for controller-ids and
     maps CC events to relative changes.
   - inputs: (list (:controller-id <id> :delta-percent <delta-percent> :turn-speed <speed>)) 
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
  (let ((input-handlers nil) (cur-cv cv-initial) (cv-range (abs (- cv-max cv-min))))
    (dolist (input inputs)
      (let ((ctrl-id (funcall (getf midi-controller :get-controller-number) (getf input :controller-id)))
	    (delta (* cv-range (getf input :delta-percent)))
	    (turn-speed (if (getf input :turn-speed) (getf input :turn-speed) (lambda (offs) offs))))
	(push (lambda (midi-event-controller-id midi-event-controller-offset)
		(if (eq midi-event-controller-id ctrl-id)
		    (let ((speed (* (signum midi-event-controller-offset)
				    (funcall turn-speed (abs midi-event-controller-offset)))))
		      (* delta speed))
		    0))
	      input-handlers)))
    (list
     :update (lambda (midi-events)
	       (let ((delta 0))
		 (dolist (midi-event midi-events)
		   (if (cl-synthesizer-midi-event:control-change-eventp midi-event)
		       (let ((controller-number (cl-synthesizer-midi-event:get-controller-number midi-event))
			     (offset (funcall (getf midi-controller :get-controller-value-offset)
					    (cl-synthesizer-midi-event:get-controller-value midi-event))))
			 (dolist (input-handler input-handlers)
			   (setf delta (+ delta (funcall input-handler controller-number offset)))))))
		 (let ((tmp (+ cur-cv delta)))
		   ;; apply clipping
		   (if (> cv-min tmp) ;; tmp < cv-min
		       (setf cur-cv cv-min)
		       (if (< cv-max tmp) ;; tmp > cv-max
			   (setf cur-cv cv-max)
			   (setf cur-cv tmp))))))
     :get-output (lambda () cur-cv))))
