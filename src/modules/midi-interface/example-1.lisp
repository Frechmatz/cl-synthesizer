(defpackage :cl-synthesizer-modules-midi-interface-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-midi-interface-example-1)

(defparameter *attach-midi* t)
(defparameter *attach-audio* t)

(defun example ()
  "Very simple midi-interface example that does not care about the gate signal."
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :input-sockets '(:midi-events)
	       :output-sockets '(:line-out))))

    (cl-synthesizer:add-module
     rack "MIDI-IFC" #'cl-synthesizer-modules-midi-interface:midi-interface
     :voice-count 1)

    (cl-synthesizer:add-module
     rack "VCO-1"
     #'cl-synthesizer-modules-vco:vco-exponential
     :base-frequency (cl-synthesizer-midi:get-note-number-frequency 0)
     :f-max 13000
     :v-peak 5)
    
    (cl-synthesizer:add-patch rack "INPUT" :midi-events "MIDI-IFC" :midi-events)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :cv-1 "VCO-1" :cv)
    (cl-synthesizer:add-patch rack "VCO-1" :saw "OUTPUT" :line-out)
    rack))

#|
(cl-synthesizer::play-rack (example) 10 
    :attach-audio t :audio-output-sockets '(:line-out) 
    :attach-midi t :midi-input-socket :midi-events)
|#
