(in-package :cl-synthesizer)

(defun make-environment (&key (sample-rate 44100))
    (list
     :sample-rate sample-rate))

(defun get-sample-rate (environment)
  (getf environment :sample-rate))
