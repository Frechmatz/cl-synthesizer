(in-package :cl-synthesizer-vendor-cc-handler)

(defun clip-7-bit (v)
  (cond
    ((> v 127)
     127)
    ((< 0 v)
     0)
    (t v)))

(defun 7-bit-relative (vendor controller-id &key (cv-initial 2.5) (cv-min 0) (cv-max 5))
  (let* ((controller-number (cl-synthesizer-vendor:get-control-number vendor controller-id)) 
	 (converter (cl-synthesizer-core:linear-converter
		     :input-min 0
		     :input-max 127
		     :output-min cv-min
		     :output-max cv-max))
	 (controller-state (funcall (getf converter :get-x) cv-initial))
	 (cur-value cv-initial))
    (list
     :update 
     (lambda (midi-events)
       (dolist (midi-event midi-events)
	 (if (and midi-event
		  (cl-synthesizer-midi-event:control-change-eventp midi-event)
		  (eq controller-number (cl-synthesizer-midi-event:get-control-number midi-event)))
	     (setf controller-state
		   (+ (cl-synthesizer-vendor:get-controller-value-offset
		      vendor
		      (cl-synthesizer-midi-event:get-control-value midi-event))))))
       (setf controller-state (clip-7-bit controller-state))
       (setf cur-value (funcall (getf converter :get-y) controller-state)))
     :get-output
     (lambda ()
       cur-value))))

