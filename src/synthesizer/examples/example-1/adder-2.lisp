(defpackage :cl-synthesizer-example-1-adder-2
  (:use :cl)
  (:export :make-module))
(in-package :cl-synthesizer-example-1-adder-2)

(defun make-module (name environment)
  "Adder module with 2 inputs"
  (declare (ignore name environment))
  (let ((input-1 nil) (input-2 nil) (cur-output nil))
    (let ((inputs (list
		   :input-1 (list
			     :set (lambda (value) (setf input-1 value))
			     :get (lambda() input-1))
		   :input-2 (list
			     :set (lambda (value) (setf input-2 value))
			     :get (lambda() input-2))))
	  (outputs (list
		    :sum (list :get (lambda() cur-output)))))
      (list
       :inputs (lambda() inputs)
       :outputs (lambda() outputs)
       :update (lambda () (setf cur-output (+ input-1 input-2)))))))
