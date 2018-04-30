(in-package :cl-synthesizer-vendor)

(defun get-controller-number (device-settings id)
  (let ((encoder-list (getf device-settings :ENCODER-CONTROLLER-NUMBERS)))
    (let ((encoder (getf encoder-list id)))
      (let ((controller-number (getf encoder :CONTROLLER-NUMBER)))
	(if (not controller-number)
	    (format t "Controller not found: ~a" id))
	controller-number))))


(defun get-controller-value-offset (device-settings controller-value)
  (funcall (getf device-settings :RELATIVE-ENCODER-OFFSET) controller-value))
