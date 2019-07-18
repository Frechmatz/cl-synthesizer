(in-package :cl-synthesizer-core)

(defun normalized-exp (value)
  "Normalized exponential function.
   value -- A number between 0..1
   Returns a number between 0..1 representing an exponential mapping of value"
  (declare (type single-float value))
  (/
   (- (expt 2 (* 8.0 value)) 1.0) ;; 0..255
   255.0))

