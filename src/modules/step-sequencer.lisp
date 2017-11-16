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
  (declare (ignore environment))
  (let* ((current-output 0)
	(inputs (list :trigger))
	(outputs (list :out))
	;; output voltages
	(steps-initial-contents '(1 2 3))
	(steps (make-array (length steps-initial-contents) :initial-contents steps-initial-contents))
	(cur-step 0)
	;; 5 Volts difference for now
	(trigger-core (cl-synthesizer-core:trigger :delta 5))
	)
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () inputs)
     :outputs (lambda () outputs)
     :get-output (lambda (output)
		   (declare (ignore output))
		   current-output)
     :update (lambda (&key (trigger 0))
	       (declare (optimize (debug 3) (speed 0) (space 0)))
	       ;; (break)
	       (if (funcall (getf trigger-core :is-trigger) trigger)
		   (progn
		     ;;(break)
		     (setf cur-step (+ cur-step 1))
		     ;; round robin
		     (if (>= cur-step (length steps-initial-contents))
			 (setf cur-step 0))))
	       (let ((r (aref steps cur-step)))
		 (setf current-output r)
		 ;;(if (= cur-step 2)
		 ;;    (break))
		 r)))))

