(defpackage :cl-synthesizer-profiling-monitor
  (:use :cl))

(in-package :cl-synthesizer-profiling-monitor)

(defun monitor-agent (name environment inputs &rest rest)
  (declare (ignore name environment inputs rest))
  (let ((input-sockets
	 (list
	  :input-1 (lambda(value) (declare (ignore value)) nil)
	  :input-2 (lambda(value) (declare (ignore value)) nil)
	  :input-3 (lambda(value) (declare (ignore value)) nil)))
	(output-sockets
	 (list
	       :input-1 (lambda() nil)
	       :input-2 (lambda() nil)
	       :input-3 (lambda() nil))))
    (values
     (list
      :inputs (lambda() input-sockets)
      :outputs (lambda() output-sockets)
      :update (lambda () nil))
   '(:input-1 :input-2 :input-3))))

(defun make-test-rack ()
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :input-sockets '(:input))))
    (cl-synthesizer:add-module
     rack "MULTIPLE"
     #'cl-synthesizer-modules-multiple:make-module
     :output-count 2)
    
    (cl-synthesizer:expose-input-socket rack :input "MULTIPLE" :input)
    (cl-synthesizer-monitor:add-monitor
     rack
     #'monitor-agent
     '(("MULTIPLE" :output-socket :output-1)
       ("MULTIPLE" :output-socket :output-2)
       ("MULTIPLE" :input-socket :input)))
     
  rack))

