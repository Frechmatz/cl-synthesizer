(in-package :cl-synthesizer-test)

(defun input-adder-module (name environment)
  "Module that adds its inputs."
  (declare (ignore environment name))
  (let ((out 0))
    (list
     :inputs (lambda () (list :in-1 :in-2))
     :outputs (lambda () (list :out))
     :get-output (lambda (output)
		     (cond 
		       ((eq :out output)
			out)
		       (t (error (format nil "Unknown output ~a requested from input-adder-module" output)))))
     :update (lambda (input-args
		      ;;&key in-1 in-2
			  )
	       (let ((in-1 (getf input-args :in-1))
		     (in-2 (getf input-args :in-2)))
	       (setf out (+ in-1 in-2)))
     ))))
