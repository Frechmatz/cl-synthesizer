(defpackage :cl-synthesizer-profiling-monitor
  (:use :cl))

(in-package :cl-synthesizer-profiling-monitor)

(defun monitor-handler (name environment inputs &rest rest)
  (declare (ignore name environment inputs rest))
  (values
   (list
    :update (lambda (args) (declare (ignore args))))
   (list :input-1 :input-2)))

(defun make-test-rack ()
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :input-sockets '(:input))))
    (cl-synthesizer:add-module
     rack "MULTIPLE"
     #'cl-synthesizer-modules-multiple:make-module
     :output-count 2)
    
    (cl-synthesizer:add-patch rack "INPUT" :input "MULTIPLE" :input)
    (cl-synthesizer-monitor:add-monitor
     rack
     #'monitor-handler
     '(("MULTIPLE" :output-socket :output-1)
       ("MULTIPLE" :output-socket :output-2)
       ("MULTIPLE" :input-socket :input)))
     
  rack))

