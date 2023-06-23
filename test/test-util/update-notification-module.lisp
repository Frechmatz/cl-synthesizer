(in-package :cl-synthesizer-test)

(defun update-notification-module (name environment &key update-handler)
  "Module that exposes 2 inputs, 2 outputs, and calls on update a lambda"
  (declare (ignore environment))
  (list
   :inputs (lambda ()
	       (list
		:input-1 (list
			  :set (lambda(value)
				 (declare (ignore value))
				 nil)
			  :get (lambda() nil))
		:input-2 (list
			  :set (lambda(value)
				 (declare (ignore value))
				 nil)
			  :get (lambda() nil))))
     :outputs (lambda ()
		(list
		 :output-1 (list :get (lambda() nil))
		 :output-2 (list :get (lambda() nil))))
   :update (lambda ()
	     (funcall update-handler name))))

