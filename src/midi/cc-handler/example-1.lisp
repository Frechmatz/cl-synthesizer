(defpackage :cl-synthesizer-modules-midi-cc-handler-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-midi-cc-handler-example-1)

(defparameter *attach-midi* t)
(defparameter *attach-audio* t)

(defun example ()
  "Modulate frequency via one controller"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :input-sockets '(:midi-events)
	       :output-sockets '(:line-out)
	       )))
    (cl-synthesizer:add-module
     rack "MIDI-IFC" #'cl-synthesizer-modules-midi-interface:midi-interface
     :voice-count 1
     :controllers
     (list
      (list :socket :controller-1
	    :handler (cl-synthesizer-midi:relative-cc-handler
		      cl-synthesizer-vendor:*arturia-minilab-mk2*
		      (list (list :controller-id :ENCODER-1 :weight 0.01
				  :turn-speed (lambda(offs) (declare (ignore offs)) 1)))
		      :cv-initial 0
		      :cv-min 0
		      :cv-max 5))))
    
    (cl-synthesizer:add-module
     rack "VCO-1"
     #'cl-synthesizer-modules-vco:vco-linear
     :base-frequency 440
     :f-max 5000
     :cv-max 5
     :v-peak 5)
    
    (cl-synthesizer:add-patch rack "INPUT" :midi-events "MIDI-IFC" :midi-events)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :controller-1 "VCO-1" :cv)
    (cl-synthesizer:add-patch rack "VCO-1" :saw "OUTPUT" :line-out)
    rack))

#|
(cl-synthesizer::play-rack (example) 10 
    :attach-audio t :audio-output-sockets '(:line-out) 
    :attach-midi t :midi-input-socket :midi-events)
|#
