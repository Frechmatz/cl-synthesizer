(defpackage :blueprint-module
  (:use :cl)
  (:export
   :make-module))

(in-package :blueprint-module)

(defun make-module (name environment &key module-param)
  (let ((input-1 nil) (input-2 nil) (output-1 nil) (output-2 nil))
    (let ((inputs
	   (list
	    :input-1 (lambda(value) (setf input-1 value))
	    :input-2 (lambda(value) (setf input-2 value))))
	  (outputs
	   (list
	    :output-1 (lambda() output-1)
	    :output-2 (lambda() output-2))))
      (list
       :inputs (lambda () inputs)
       :outputs (lambda () outputs)
       :update (lambda ()
		 (setf output-1 input-1)
		 (setf output-2 input-2))
       :get-state (lambda (key)
		    (if (eq key :module-param) module-param nil))
       :shutdown (lambda ())))))
