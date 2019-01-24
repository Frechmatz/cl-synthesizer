(in-package :cl-synthesizer-test)

(defun is-approximately (expected-number number allowed-deviation)
  "Returns t if a given number is within a given range.
   - expected-number The expected number.
   - number The actual number.
   - allowed-deviation The absolute value of the allowed deviation.
   Examples:
    4.0    4.0001    0.01 => t
   -4.0    4.0001    0.01 => nil
    4.0   -4.0001    0.01 => nil
    0.0    0.001     0.01 => t
    0.0    0.1       0.01 => nil"
  (let ((diff (- expected-number number)))
    (< (abs diff) allowed-deviation)))

 
