(in-package :cl-synthesizer-test)

(defun pass-through-module (name environment)
  "Module that passes through inputs (:cv-1 :cv-2) to outputs (:out-1 :out-2)"
  (declare (ignore environment))
  (let ((out-1 0) (out-2 0))
    (list
     :get-state (lambda(state)
		  (if (eq state :module-name)
		      name
		      (error (format nil "Unknown state ~a requested frm module pass-through-module" state))))
     :inputs (lambda () (list :cv-1 :cv-2))
     :outputs (lambda () (list :out-1 :out-2))
     :get-output (lambda (output)
		     (cond 
		       ((eq :out-1 output)
			out-1)
		       ((eq :out-2 output)
			out-2)
		       (t (error (format nil "Unknown output ~a requested from pass-through-module" output)))))
     :update (lambda (input-args)
	       (setf out-1 (getf input-args :cv-1))
	       (setf out-2 (getf input-args :cv-2))))))

