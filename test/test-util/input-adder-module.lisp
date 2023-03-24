(in-package :cl-synthesizer-test)

(defun input-adder-module (name environment)
  "Module that adds its inputs (:in-1 :in-2) and exposes the sum via output :out."
  (declare (ignore environment))
  (let ((out 0) (in-1 nil) (in-2 nil))
    (list
     :state (lambda(state)
		  (if (eq state :module-name)
		      name
		      (error (format nil "Unknown state '~a' requested frm module input-adder-module" state))))
     :inputs (lambda ()
	       (list
		:in-1 (list
		       :set (lambda(value) (setf in-1 value))
		       :get (lambda() in-1))
		:in-2 (list
		       :set (lambda(value) (setf in-2 value))
		       :get (lambda() in-2))))
     :outputs (lambda ()
		(list :out (list :get (lambda() out))))
     :update (lambda ()
	       (setf out (+ in-1 in-2))))))
