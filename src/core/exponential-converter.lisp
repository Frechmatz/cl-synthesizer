(in-package :cl-synthesizer-core)


(defun exponential-converter (&key base-value)
  ""
  (list
   :get-y
   (lambda (input-value)
     ;; f_0 * power (2, cv)
     (* base-value (expt 2 input-value)))
   :get-x
   (lambda (output-value)
     (declare (ignore output-value))
     (error "Inverse function of exponential converter is not implemented"))))


     
