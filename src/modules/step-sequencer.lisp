;;
;;
;; A step sequencer
;;
;; Work in progress
;;
;;

(in-package :cl-synthesizer-modules-step-sequencer)

(defun step-sequencer (environment)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let* ((current-output 0)
	 ;; output voltages
	 (steps-initial-contents '(1 2 3))
	 (steps (make-array (length steps-initial-contents) :initial-contents steps-initial-contents))
	 (step-loggers (make-array (length steps-initial-contents) :initial-contents steps-initial-contents))
	 (cur-step nil)
	 (trigger-core (cl-synthesizer-core:trigger :switching-voltage 4.9)))
    ;; Set up event loggers
    (dotimes (i (length steps-initial-contents))
      (setf (aref step-loggers i)
	    (funcall (getf environment :event-logger-register-event-type)
		     (format nil "STEP-~a" i))))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () '(:step))
     :outputs (lambda () '(:out))
     :get-output (lambda (output)
		   (declare (ignore output))
		   current-output)
     :update (lambda (&key (step 0))
	       (declare (optimize (debug 3) (speed 0) (space 0)))
	       (if (funcall (getf trigger-core :is-firing) step)
		   (progn
		     (setf cur-step (if cur-step (+ cur-step 1) 0))
		     ;; round robin
		     (if (<= (length steps) cur-step )
			 (setf cur-step 0))
		     (funcall (elt step-loggers cur-step))
		     (setf current-output (elt steps cur-step))))))))
     
