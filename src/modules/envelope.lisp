;;
;;
;; Generic Envelope Generator
;; Will replace the ADSR module
;;
;; Work in progress
;;
;;

(in-package :cl-synthesizer-modules-envelope)

(defun validate-segment (required-gate-state target-cv time-ms)
  (if (and (eq :ignore required-gate-state) (not time-ms))
      (cl-synthesizer:signal-assembly-error
       :format-control "If required-gate-state is :ignore then time-ms must not be nil"
       :format-arguments (list)))
  (if (and target-cv (not time-ms) (eq :ignore required-gate-state))
      (cl-synthesizer:signal-assembly-error
       :format-control "target-cv set to ~a but time-ms is nil and required-gate-state is :ignored"
       :format-arguments (list target-cv))))

(defun envelope (name environment &key segments)
  (declare (ignore name))
  (let ((segment-def nil)
	(is-gate nil)
	(cur-cv 0)
	(ticks-per-ms (floor (/ (getf environment :sample-rate) 1000))))
    (dolist (segment segments)
      (let ((update-fn nil)
	    (required-gate-state (getf segment :required-gate-state))
	    (target-cv (getf segment :target-cv))
	    (time-ms (getf segment :time-ms)))
	(validate-segment required-gate-state target-cv time-ms)
	(push 
	 (list
	  :init (lambda ()
		  (cond
		    ((and time-ms (= 0 time-ms))
		     (setf update-fn (lambda () :CONTINUE))) 
		    ((and target-cv time-ms)
		     (let* ((elapsed-ticks 0)
			    (total-ticks (* ticks-per-ms time-ms))
			    (converter (cl-synthesizer-core:linear-converter
					:input-min 0
					:input-max total-ticks
					:output-min cur-cv
					:output-max target-cv)))
		       (setf update-fn
			     (lambda ()
			       (setf elapsed-ticks (+ 1 elapsed-ticks))
			       (cond
				 ((> elapsed-ticks total-ticks)
				  :CONTINUE)
				 ((and (eq :on required-gate-state) (not is-gate))
				  :CONTINUE)
				 ((and (eq :off required-gate-state) is-gate)
				  :CONTINUE)
				 (t
				  (setf cur-cv (funcall (getf converter :get-y) elapsed-ticks))
				  :DONE))))))
		    (target-cv
		     (setf update-fn
			   (lambda ()
			     (cond
			       ((and (eq :on required-gate-state) (not is-gate))
				:CONTINUE)
			       ((and (eq :off required-gate-state) is-gate)
				:CONTINUE)
			       (t
				(setf cur-cv target-cv)
				:DONE)))))
		    (t
		     (setf update-fn
			   (lambda ()
			     (cond
			       ((and (eq :on required-gate-state) (not is-gate))
				:CONTINUE)
			       ((and (eq :off required-gate-state) is-gate)
				:CONTINUE)
			       (t
				:DONE)))))))
	  :update (lambda() (funcall update-fn)))
	 segment-def)))
    (let ((controller (cl-synthesizer-core:function-array (reverse segment-def))))
      (list
       :inputs (lambda () '(:gate))
       :outputs (lambda () '(:cv))
       :get-output (lambda (output)
		     (declare (ignore output))
		     cur-cv)
       :update (lambda (&key (gate 0))
		 (let ((previous-gate is-gate) (restart nil))
		   (setf is-gate (if (>= gate 4.9) t nil))
		   (setf restart (and is-gate (not previous-gate)))
		   (if restart
		       (setf cur-cv 0))
		   (funcall controller restart)))))))

