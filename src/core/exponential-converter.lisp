(in-package :cl-synthesizer-core)


(defun exponential-converter (&key base-value)
  ""
  (list
   :input-to-output ;; x -> y
   (lambda (input-value)
     ;; f_0 * power (2, cv)
     (* base-value (expt 2 input-value)))
   :output-to-input ;; y -> x
   (lambda (output-value)
     (declare (ignore output-value))
     (error "Inverse function of exponential converter is not implemented"))))


     
