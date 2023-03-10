(defpackage :cl-synthesizer-example-1-adder-2
  (:use :cl)
  (:export :make-module))
(in-package :cl-synthesizer-example-1-adder-2)

(defun make-module (name environment)
  "Adder module with 2 inputs"
  (declare (ignore name environment))
  (let ((input-1 nil) (input-2 nil) (cur-output nil))
    (let ((inputs (list
		   :input-1 (lambda (value) (setf input-1 value))
		   :input-2 (lambda (value) (setf input-2 value))))
	  (outputs (list
		    :sum (lambda() cur-output))))
      (list
       :inputs (lambda () inputs)
       :outputs (lambda () outputs)
       :update (lambda () (setf cur-output (+ input-1 input-2)))))))
