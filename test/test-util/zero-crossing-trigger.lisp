(in-package :cl-synthesizer-test)

(defun zero-crossing-trigger ()
  "Returns a lambda which returns t if the input signal has crossed zero."
  (let ((cur-value 0.0))
    (lambda (v)
      (let ((is-crossing
	     (or
	      (and (< cur-value 0.0) (< 0.0 v))
	      (and (< v 0.0) (< 0.0 cur-value)))))
	(setf cur-value v)
	is-crossing))))
