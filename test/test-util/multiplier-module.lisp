(in-package :cl-synthesizer-test)

(defun multiplier-module (name environment)
  "Module that multiplies its input :in by two and exposes the result via output :out"
  (declare (ignore environment name))
  (let ((out 0))
    (list
     :inputs (lambda () (list :in))
     :outputs (lambda () (list :out))
     :get-output (lambda (output)
		     (cond 
		       ((eq :out output)
			out)
		       (t (error (format nil "Unknown output ~a requested from multiplier-module" output)))))
     :update (lambda (input-args)
	       (setf out (* 2 (getf input-args :in)))))))
