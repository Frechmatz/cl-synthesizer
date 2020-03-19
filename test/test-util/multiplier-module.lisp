(in-package :cl-synthesizer-test)

(defun multiplier-module (name environment)
  "Module that multiplies its input :in by two and exposes the result via output :out"
  (declare (ignore environment))
  (let ((out 0) (in nil))
    (list
     :state (lambda(state)
		  (if (eq state :module-name)
		      name
		      (error (format nil "Unknown state ~a requested frm module multiplier-module" state))))
     :inputs (lambda () (list :in (lambda(value) (setf in value))))
     :outputs (lambda () (list :out (lambda() out)))
     :update (lambda ()
	       (setf out (* 2 in))))))
