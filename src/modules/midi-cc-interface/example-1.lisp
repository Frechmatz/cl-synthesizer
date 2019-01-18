(defpackage :cl-synthesizer-modules-midi-cc-interface-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-midi-cc-interface-example-1)

(defun example ()
  "MIDI CC-Interface Example"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :input-sockets '(:midi-events))))

    (cl-synthesizer:add-module
     rack "MIDI-CC-IFC" #'cl-synthesizer-modules-midi-cc-interface:make-module
     :controller-numbers '(112)
     :initial-output 2.5
     :min-output 0.0
     :max-output 5.0
     :transform-handler
     (lambda (cur-output controller-number control-value)
       (declare (ignore controller-number))
       (cond
	 ((= control-value 61)
	  (+ cur-output -0.5))
	 ((= control-value 67)
	  (+ cur-output 0.5))
	 (t cur-output)))
     :channel nil)
    
    (cl-synthesizer:add-patch rack "INPUT" :midi-events "MIDI-CC-IFC" :midi-events)

    rack))

(defun run-example ()
  (cl-synthesizer::play-rack (example) 5))

;; (run-example)
