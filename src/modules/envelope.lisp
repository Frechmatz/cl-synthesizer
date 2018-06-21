;;
;;
;; Generic Envelope Generator
;; Will replace the ADSR module
;;
;; Work in progress
;;
;;

(in-package :cl-synthesizer-modules-envelope)

;; todo: what is the meaning of the params?
(defun has-segment-completed (total-ticks elapsed-ticks is-gate required-gate-state)
  (cond 
    ((and elapsed-ticks total-ticks (>= elapsed-ticks total-ticks))
     t)
    ((and (eq :on required-gate-state) (not is-gate))
     t)
    ((and (eq :off required-gate-state) is-gate)
     t)
    (t nil)))

(defun validate-segment (required-gate-state target-cv time-ms)
  (if (and (eq :ignore required-gate-state) (not time-ms))
      (cl-synthesizer:signal-assembly-error
       :format-control "If required-gate-state is :ignore then time-ms must not be nil"
       :format-arguments (list)))
  (if (and target-cv (not time-ms))
      (cl-synthesizer:signal-assembly-error
       :format-control "target-cv set to ~a but time-ms is nil"
       :format-arguments (list target-cv))))

(defun envelope (name environment &key segments)
  (declare (ignore name))
  (let ((segment-def nil)
	(is-gate nil)
	(cur-cv 0)
	(ticks-per-ms (floor (/ (getf environment :sample-rate) 1000))))
    (dolist (segment segments)
      (let ((total-ticks nil) (elapsed-ticks nil) (transfer-fn nil)
	    ;;(name (getf segment :name))
	    (required-gate-state (getf segment :required-gate-state))
	    (target-cv (getf segment :target-cv))
	    (time-ms (getf segment :time-ms)))
	(validate-segment required-gate-state target-cv time-ms)
	(push 
	 (list
	  :init (lambda ()
		  (setf elapsed-ticks -1)
		  (setf total-ticks (if time-ms (* ticks-per-ms time-ms) nil))
		  (setf transfer-fn
			;; todo: Only re-create fn if target-cv or total-ticks has changed
			(if target-cv
			    (getf (cl-synthesizer-core:linear-converter
				   :input-min 0
				   :input-max total-ticks
				   :output-min cur-cv
				   :output-max target-cv)
				  :get-y)
			    (lambda (elapsed-ticks)
			      (declare (ignore elapsed-ticks))
			      cur-cv))))
	  :update (lambda()
		    (setf elapsed-ticks (+ 1 elapsed-ticks))
		    (if (has-segment-completed total-ticks elapsed-ticks is-gate required-gate-state)
			:CONTINUE
			(progn
			  (setf cur-cv (funcall transfer-fn elapsed-ticks))
			  :DONE))))
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

