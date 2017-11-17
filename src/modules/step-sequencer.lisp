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
	;; output voltages
	(steps-initial-contents '(1 2 3))
	(steps (make-array (length steps-initial-contents) :initial-contents steps-initial-contents))
	(cur-step 0)
	(trigger-core (cl-synthesizer-core:trigger :switching-voltage 4.9))
	)
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () '(:step))
     :outputs (lambda () '(:out))
     :get-output (lambda (output)
		   (declare (ignore output))
		   current-output)
     :update (lambda (&key (step 0))
	       (declare (optimize (debug 3) (speed 0) (space 0)))
	       ;; (break)
	       (if (funcall (getf trigger-core :is-firing) step)
		   (progn
		     ;;(break)
		     (setf cur-step (+ cur-step 1))
		     ;; round robin
		     (if (>= cur-step (length steps-initial-contents))
			 (setf cur-step 0))
		     (format t "~%Step: ~a out: ~a" cur-step (aref steps cur-step)) ))
	       (let ((r (aref steps cur-step)))
		 (setf current-output r)
		 ;;(if (= cur-step 2)
		 ;;    (break))
		 r)))))

