

(in-package :cl-synthesizer)

(defun event-logger (name event-printer)
  (list
   :register-event-type
   (lambda (event-name &key (latency 0))
     (declare (ignore latency))
     (lambda ()
       (funcall event-printer name event-name 1)))
   :clear (lambda ())
   :tick (lambda ())
  ))
