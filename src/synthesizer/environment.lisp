(in-package :cl-synthesizer)

(defun make-environment (&key (sample-rate 44100))
  (let ((logger (cl-synthesizer-event-logger:event-logger)))
    (list
     :sample-rate sample-rate
     :event-logger logger
     :register-event (getf logger :register-event))))

(defun get-event-logger (environment)
  (getf environment :event-logger))

(defun get-sample-rate (environment)
  (getf environment :sample-rate))
