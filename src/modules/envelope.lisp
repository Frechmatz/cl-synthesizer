;;
;;
;; Generic Envelope Generator
;;
;; Work in progress
;;
;;

(in-package :cl-synthesizer-modules-envelope)

#|
required-gate-state    target-cv     duration-ms   Action
:ignore                nil           nil           Error
:ignore                nil           t             Ok
:ignore                t             nil           Error
:ignore                t             t             Ok
:on                    *             *             Ok
:off                   *             *             Ok
|#
(defun validate-segment (segment)
  "Perform some basic plausibility checks of a segment definition"
  (let ((required-gate-state (getf segment :required-gate-state))
	(target-cv (getf segment :target-cv))
	(duration-ms (getf segment :duration-ms)))
    (cond
      ((and (eq :ignore required-gate-state) (not target-cv) (not duration-ms))
       (cl-synthesizer:signal-assembly-error
	:format-control "Invalid segment: ~a"
	:format-arguments (list segment)))
      ((and (eq :ignore required-gate-state) target-cv (not duration-ms))
       (cl-synthesizer:signal-assembly-error
	:format-control "Invalid segment: ~a"
	:format-arguments (list segment))))))

(defmacro with-gate-check (&body body)
  `(cond
    ((and (eq :on required-gate-state) (not is-gate))
     :CONTINUE)
    ((and (eq :off required-gate-state) is-gate)
     :CONTINUE)
    (t
     ,@body)))

(defun envelope (name environment &key segments (gate-trigger-threshold-cv 4.9))
  (declare (ignore name))
  (let ((segment-def nil)
	(is-gate nil)
	(cur-cv 0)
	(ticks-per-ms (floor (/ (getf environment :sample-rate) 1000))))
    (dolist (segment segments)
      (validate-segment segment)
      (let ((update-fn nil)
	    (required-gate-state (getf segment :required-gate-state))
	    (target-cv (getf segment :target-cv))
	    (duration-ms (getf segment :duration-ms)))
	(push 
	 (list
	  :init (lambda ()
		  (cond
		    ((and duration-ms (= 0 duration-ms))
		     (setf update-fn (lambda () :CONTINUE))) 
		    ((and target-cv duration-ms)
		     (let* ((elapsed-ticks 0)
			    (total-ticks (* ticks-per-ms duration-ms))
			    (converter (cl-synthesizer-core:linear-converter
					:input-min 0
					:input-max total-ticks
					:output-min cur-cv
					:output-max target-cv)))
		       (setf update-fn
			     (lambda ()
			       (setf elapsed-ticks (+ 1 elapsed-ticks))
			       (if (> elapsed-ticks total-ticks)
				  :CONTINUE
				 (with-gate-check
				   (setf cur-cv (funcall (getf converter :get-y) elapsed-ticks))
				   :DONE))))))
		    (target-cv
		     (setf update-fn
			   (lambda ()
			     (with-gate-check
			       (setf cur-cv target-cv)
			       :DONE))))
		    (t
		     (setf update-fn
			   (lambda ()
			     (with-gate-check 
			       :DONE))))))
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
		   (setf is-gate (if (>= gate gate-trigger-threshold-cv) t nil))
		   (setf restart (and is-gate (not previous-gate)))
		   (if restart
		       (setf cur-cv 0))
		   (funcall controller restart)))))))

