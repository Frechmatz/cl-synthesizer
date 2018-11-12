(defpackage :cl-synthesizer-modules-midi-cc-interface-example-2
  (:use :cl))

(in-package :cl-synthesizer-modules-midi-cc-interface-example-2)

(defparameter *attach-midi* t)
(defparameter *attach-speaker* t)

(defun example ()
  "Modulate frequency via two CC controllers"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :input-sockets '(:midi-events)
	       :output-sockets '(:line-out)))
	(lsb-controller-number
	 (funcall
	  (getf cl-synthesizer-vendor:*arturia-minilab-mk2* :get-controller-number) :encoder-1))
	(msb-controller-number
	 (funcall
	  (getf cl-synthesizer-vendor:*arturia-minilab-mk2* :get-controller-number) :encoder-9)))
    
    (cl-synthesizer:add-module
     rack "MIDI-CC-IFC" #'cl-synthesizer-modules-midi-cc-interface:make-module
     :controller-numbers (list msb-controller-number lsb-controller-number)
     :initial-output 0.0
     :min-output 0.0
     :max-output 5.0
     :channel nil
     :transform-handler
     (lambda (cur-output controller-number control-value)
       (let ((offs 
	      (cond
		((= 61 control-value) -0.01)
		((= 62 control-value) -0.02)
		((= 63 control-value) -0.03)
		((= 65 control-value) 0.01)
		((= 66 control-value) 0.02)
		((= 67 control-value) 0.03)
		(t 0))))
	 (if (= controller-number msb-controller-number)
	     (setf offs (* 2.0 offs)))
	 (let ((r (+ cur-output offs)))
	   ;;(format t "CV: ~a~%" r)
	   r))))
    
    (cl-synthesizer:add-module
     rack "VCO-1"
     #'cl-synthesizer-modules-vco:make-linear-module
     :base-frequency 440
     :cv-max 5
     :f-max 5000
     :v-peak 5)
    
    (cl-synthesizer:add-patch rack "INPUT" :midi-events "MIDI-CC-IFC" :midi-events)
    (cl-synthesizer:add-patch rack "MIDI-CC-IFC" :output "VCO-1" :cv)
    (cl-synthesizer:add-patch rack "VCO-1" :sine "OUTPUT" :line-out)
    rack))

#|
(cl-synthesizer::play-rack (example) 30 
    :attach-audio t :audio-output-sockets '(:line-out) 
    :attach-midi t :midi-input-socket :midi-events)
|#
