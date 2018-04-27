(in-package :cl-synthesizer-vendor)

(defun get-controller-number (vendor id)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((encoder-list (getf vendor :ENCODER-CONTROLLER-NUMBERS)))
    (let ((encoder (getf encoder-list id)))
      (let ((controller-number (getf encoder :CONTROLLER-NUMBER)))
	(if (not controller-number)
	    (format t "Controller not found: ~a" id))
	controller-number))))


(defun get-controller-value-offset (vendor controller-value)
  (funcall (getf vendor :RELATIVE-ENCODER-OFFSET) controller-value))
