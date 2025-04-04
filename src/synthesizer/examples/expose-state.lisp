(defpackage :cl-synthesizer-examples-expose-state
  (:use :cl)
  (:export :make-module))
(in-package :cl-synthesizer-examples-expose-state)

(defun make-module (name environment)
  "Module that exposes an internal state."
  (declare (ignore name environment))
  (let ((internal-state 0))
    (list
     :inputs (lambda() nil)
     :outputs (lambda() nil)
     :update (lambda () (setf internal-state (+ 1 internal-state)))
     :state (lambda (key)
	      (cond
		((eq key :state)
		 internal-state)
		(t nil))))))
