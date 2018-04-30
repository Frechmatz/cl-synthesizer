(in-package :cl-synthesizer-vendor-cc-handler)

(defun clip-127 (v)
  (cond
    ((<= 127 v)
     127)
    ((> 0 v)
     0)
    (t v)))

(defun clip-16383 (v)
  (cond
    ((<= 16383 v)
     16383)
    ((> 0 v)
     0)
    (t v)))

(defun 7-bit-relative (device-settings controller-id &key (cv-initial 2.5) (cv-min 0) (cv-max 5))
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let* ((controller-number (cl-synthesizer-vendor:get-controller-number device-settings controller-id)) 
	 (converter (cl-synthesizer-core:linear-converter
		     :input-min 0
		     :input-max 127
		     :output-min cv-min
		     :output-max cv-max))
	 (controller-state (funcall (getf converter :get-x) cv-initial))
	 (cur-value cv-initial))
    (if (not controller-number)
	(cl-synthesizer:signal-assembly-error
	 :format-control "~%Controller id not supported by device-settings" 
	 :format-arguments (list controller-id)))
    (list
     :update 
     (lambda (midi-events)
       (declare (optimize (debug 3) (speed 0) (space 0)))
       (let ((found nil))
	 (dolist (midi-event midi-events)
	   (if (and midi-event
		    (cl-synthesizer-midi-event:control-change-eventp midi-event)
		    (eq controller-number (cl-synthesizer-midi-event:get-controller-number midi-event)))
	       (progn
		 (setf found t)
		 (setf controller-state
		       (+
			controller-state
			(cl-synthesizer-vendor:get-controller-value-offset
			 device-settings
			 (cl-synthesizer-midi-event:get-controller-value midi-event)))))))
	 (if found
	     (progn
	       (setf controller-state (clip-127 controller-state))
	       (setf cur-value (funcall (getf converter :get-y) controller-state))
	       (format t "~%Updated controller state. CV is ~a" cur-value)
	       )
	     
	     )))
       :get-output
     (lambda ()
       cur-value))))

(defun 14-bit-relative (device-settings &key controller-id-msb controller-id-lsb (cv-initial 2.5) (cv-min 0) (cv-max 5))
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let* ((controller-number-msb (cl-synthesizer-vendor:get-controller-number device-settings controller-id-msb))
	 (controller-number-lsb (cl-synthesizer-vendor:get-controller-number device-settings controller-id-lsb))
	 (converter (cl-synthesizer-core:linear-converter
		     :input-min 0
		     :input-max (+ (* 128 128) -1)
		     :output-min cv-min
		     :output-max cv-max))
	 (controller-state (funcall (getf converter :get-x) cv-initial)))
    (if (not controller-number-msb)
	(cl-synthesizer:signal-assembly-error
	 :format-control "Controller id not supported by device-settings" 
	 :format-arguments (list controller-id-msb)))
    (if (not controller-number-lsb)
	(cl-synthesizer:signal-assembly-error
	 :format-control "Controller id not supported by device-settings" 
	 :format-arguments (list controller-id-lsb)))
    (if (eq controller-number-msb controller-number-lsb)
	(cl-synthesizer:signal-assembly-error
	 :format-control "Controllers must be assigned to different controller numbers: %s %s" 
	 :format-arguments (list controller-id-msb controller-id-lsb)))
    (flet ((controller-state-to-cv ()
	     (clip-16383 (funcall (getf converter :get-y) controller-state)))
	   (get-event-offset (midi-event)
	     (cl-synthesizer-vendor:get-controller-value-offset
	      device-settings
	      (cl-synthesizer-midi-event:get-controller-value midi-event))))
	   (let ((cur-value (controller-state-to-cv)))
	     (list
	      :update 
	      (lambda (midi-events)
		(declare (optimize (debug 3) (speed 0) (space 0)))
		(let ((found nil))
		  (dolist (midi-event midi-events)
		    (if (and midi-event (cl-synthesizer-midi-event:control-change-eventp midi-event))
			(cond
			  ((eq controller-number-lsb (cl-synthesizer-midi-event:get-controller-number midi-event))
			   (progn
			     (setf found t)
			     (setf controller-state (+ controller-state (get-event-offset midi-event)))))
			   ((eq controller-number-msb (cl-synthesizer-midi-event:get-controller-number midi-event))
			    (progn
			      (setf found t)
			      ;;(break)
			      (setf controller-state (+ controller-state (* 128 (get-event-offset midi-event)))))))))
		  (if found
		      (progn
			(setf cur-value (controller-state-to-cv))
			;;(format t "~%Updated controller state. CV is ~a" cur-value)
			))))
	      :get-output
	      (lambda ()
		cur-value))))))
