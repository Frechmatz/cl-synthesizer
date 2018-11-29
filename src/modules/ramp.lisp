;;
;;
;;

(in-package :cl-synthesizer-modules-ramp)

(defun make-module (name environment &key time-ms target-value)
  "TODO"
  (declare (ignore environment time-ms target-value))
  (let ((output 0.0) (busy 0.0) (done 0.0))
    (list
     :inputs (lambda () '(:trigger :input))
     ;; :busy -> Gate
     ;; :done -> Trigger
     ;; we need bot :busy and :done due to retriggering.
     :outputs (lambda () '(:output :busy :done))
     :get-output (lambda (output-socket)
		   (cond
		     ((eq :output output-socket)
		      output)
		     ((eq :busy output-socket)
		      busy)
		     ((eq :done output-socket)
		      done)
		     (t
		      (error (format nil "Output socket ~a not supported by module ~a" output-socket name)))))
     :update (lambda (&key trigger input)
	       (declare (ignore trigger input))
	       nil))))


