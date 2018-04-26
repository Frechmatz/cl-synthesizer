(in-package :cl-synthesizer-vendor)

(defun get-control-number (vendor id)
  (getf (getf (getf vendor :ENCODER-CONTROL-NUMBERS) id) :CONTROL-NUMBER))

(defun get-controller-value-offset (vendor controller-value)
  (funcall (getf vendor :RELATIVE-ENCODER-OFFSET) controller-value))
