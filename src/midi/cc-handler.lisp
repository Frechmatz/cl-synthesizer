(in-package :cl-synthesizer-midi)

(defun relative-cc-handler (midi-controller inputs &key (cv-initial 2.5) (cv-min 0) (cv-max 5))
  "Handler that converts relative MIDI CC-Events to control voltages. Can combine 
   the events of multiple controller ids.
   - midi-controller: The MIDI controller. Defines keywords for controller-ids and
     knows how to interpret relative CC events.
   - inputs: (list (:controller-id id :factor-percent factor-percent)) where factor-percent
     defines how much the control-voltage will be increased/decreased when the controller
     signals an increment/decrement. The value is relative to the total control voltage range
     as defined by cv-min and cv-max. The controller id is a keyword that must be supported
     by the given midi-controller, for example :encoder-1"
  (let ((input-handlers nil) (cur-cv cv-initial) (cv-range (abs (- cv-max cv-min))))
    (dolist (input inputs)
      (let ((ctrl-id (funcall (getf midi-controller :get-controller-number) (getf input :controller-id)))
	    (delta (* cv-range (getf input :factor-percent))))
	(push (lambda (midi-event-controller-id midi-event-controller-offset)
		(if (eq midi-event-controller-id ctrl-id)
		    (* delta midi-event-controller-offset)
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
