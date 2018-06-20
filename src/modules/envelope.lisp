;;
;;
;; Generic Envelope Generator
;; Will replace the ADSR module
;;
;; Work in progress
;;
;;

(in-package :cl-synthesizer-modules-envelope)

(defun has-segment-completed (total-ticks elapsed-ticks is-gate requires-gate)
  (cond 
    ((and requires-gate (not is-gate))
     t)
    ((and elapsed-ticks total-ticks)
     (>= elapsed-ticks total-ticks))
    (requires-gate
     nil)
    (t (error "Cannot evaluate has-segment-completed"))))

(defun validate-segment (requires-gate target-cv time-ms)
  (if (and (not requires-gate) (not time-ms))
      (cl-synthesizer:signal-assembly-error
       :format-control "One of (requires-gate time-ms) must be t"
       :format-arguments (list)))
  (if (and target-cv (not time-ms))
      (cl-synthesizer:signal-assembly-error
       :format-control "target-cv set to ~a but time-ms is nil"
       :format-arguments (list target-cv))))

(defmacro segment ((&key name requires-gate target-cv time-ms))
  "Generates a segment whose output starts at cur-cv and descends/ascends to the given target
voltage within the given time interval. Depending on the 'requires-gate' parameter the segment
terminates when the gate drops to 0."
  (declare (ignore name))
  (validate-segment requires-gate target-cv time-ms)
  `(let ((total-ticks nil) (elapsed-ticks nil) (transfer-fn nil))
     (list
      :init (lambda ()
	      (setf elapsed-ticks -1)
	      (setf total-ticks (if ,time-ms (* ticks-per-ms ,time-ms) nil))
	      (setf transfer-fn
		    ;; todo: Only re-create fn if target-cv or total-ticks has changed
		    (if ,target-cv
			(getf (cl-synthesizer-core:linear-converter
			       :input-min 0
			       :input-max total-ticks
			       :output-min cur-cv
			       :output-max ,target-cv)
			      :get-y)
			(lambda (elapsed-ticks)
			  (declare (ignore elapsed-ticks))
			  cur-cv))))
      :update (lambda()
		(setf elapsed-ticks (+ 1 elapsed-ticks))
		(if (has-segment-completed total-ticks elapsed-ticks is-gate ,requires-gate)
		    :CONTINUE
		    (progn
		      (setf cur-cv (funcall transfer-fn elapsed-ticks))
		      :DONE))))))

(defmacro envelope (segments)
  (let ((segment-def nil))
    ;; compile segments
    (dolist (segment segments)
      (push `(segment
	      (:name
	       ,(getf segment :name)
	       :requires-gate
	       ,(getf segment :requires-gate)
	       :target-cv
	       ,(getf segment :target-cv)
	       :time-ms
	       ,(getf segment :time-ms)))
	    segment-def))
    (setf segment-def (reverse segment-def))
    ;;(format t "~%Compiled segments: ~a~%" segment-def)
    `(lambda(name environment)
       (declare (ignore name))
       (let* ((is-gate nil)
	      (cur-cv 0)
	      (ticks-per-ms (floor (/ (getf environment :sample-rate) 1000)))
	      (controller (cl-synthesizer-core:function-array (list ,@segment-def))))
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
		      (funcall controller restart))))))))

