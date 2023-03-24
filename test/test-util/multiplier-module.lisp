(in-package :cl-synthesizer-test)

(defun multiplier-module (name environment)
  "Module that multiplies its input :in by two and exposes the result via output :out"
  (declare (ignore environment))
  (let ((out 0) (in nil))
    (list
     :state (lambda(state)
		  (if (eq state :module-name)
		      name
		      (error (format nil "Unknown state '~a' requested frm module multiplier-module" state))))
     :inputs (lambda () (list :in (list
				   :set (lambda(value) (setf in value))
				   :get (lambda() in))))
     :outputs (lambda () (list :out (list
				     :get (lambda() out))))
     :update (lambda ()
	       (setf out (* 2 in))))))
