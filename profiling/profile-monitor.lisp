(defpackage :cl-synthesizer-profiling-monitor
  (:use :cl))

(in-package :cl-synthesizer-profiling-monitor)

(defun monitor-agent (name environment inputs &rest rest)
  (declare (ignore name environment inputs rest))
  (let ((input-sockets
	 (list
	  :input-1 (list
		    :set (lambda(value) (declare (ignore value)) nil)
		    :get (lambda() nil))
	  :input-2 (list
		    :set (lambda(value) (declare (ignore value)) nil)
		    :get (lambda() nil))
	  :input-3 (list
		    :set (lambda(value) (declare (ignore value)) nil)
		    :get (lambda() nil))))
	(output-sockets
	 (list
	       :input-1 (list :get (lambda() nil))
	       :input-2 (list :get (lambda() nil))
	       :input-3 (list :get (lambda() nil)))))
    (values
     (list
      :inputs (lambda() input-sockets)
      :outputs (lambda() output-sockets)
      :update (lambda () nil))
   '(:input-1 :input-2 :input-3))))

(defun make-test-rack ()
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack "MULTIPLE"
     #'cl-synthesizer-modules-multiple:make-module
     :output-count 3)
    
    (cl-synthesizer:add-rack-input rack :input "MULTIPLE" :input)
    (cl-synthesizer-monitor:add-monitor
     rack
     #'monitor-agent
     '(("MULTIPLE" :output-socket :output-1)
       ("MULTIPLE" :output-socket :output-2)
       ("MULTIPLE" :output-socket :output-3)))
     
  rack))

