(in-package :cl-synthesizer-test)

(defun input-adder-module (name environment)
  "Module that adds its inputs (:in-1 :in-2) and exposes the sum via output :out."
  (declare (ignore environment))
  (let ((out 0) (in-1 nil) (in-2 nil))
    (list
     :get-state (lambda(state)
		  (if (eq state :module-name)
		      name
		      (error (format nil "Unknown state ~a requested frm module input-adder-module" state))))
     :inputs (lambda ()
	       (list
		:in-1 (lambda(value) (setf in-1 value))
		:in-2 (lambda(value) (setf in-2 value))))
     :outputs (lambda ()
		(list :out (lambda() out)))
     :update (lambda ()
	       (setf out (+ in-1 in-2))))))
