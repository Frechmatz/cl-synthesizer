(in-package :cl-synthesizer-test)

(defun update-counter-module (name environment)
  "Module with no inputs that counts invocations of the update function and exposes the count via output :out"
  (declare (ignore environment name))
  (let ((out 0))
    (list
     :inputs (lambda () nil)
     :outputs (lambda () (list :out))
     :get-output (lambda (output)
		     (cond 
		       ((eq :out output)
			out)
		       (t (error (format nil "Unknown output ~a requested from update-counter-module" output)))))
     :update (lambda (input-args)
	       (declare (ignore input-args))
	       (setf out (+ 1 out))))))
