(defpackage :cl-synthesizer-rack-example-1
  (:use :cl))

(in-package :cl-synthesizer-rack-example-1)

(defun make-voice (name environment &key lfo-frequency vco-frequency)
  "Creates a module which generates a frequency modulated saw signal."
  (declare (ignore name))
  (let ((voice
	 (cl-synthesizer:make-rack
	  :environment environment
	  ;; Expose audio output socket
	  :output-sockets '(:audio))))
    
    ;; Add LFO
    (cl-synthesizer:add-module
     voice "LFO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency lfo-frequency :v-peak 0.1 :f-max 500.0 :cv-max 5.0)

    ;; Add VCO
    (cl-synthesizer:add-module
     voice "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency vco-frequency :f-max 5000.0 :v-peak 5.0 :cv-max 5.0)

    ;; Patch LFO with VCO
    (cl-synthesizer:add-patch voice "LFO" :sine "VCO" :cv-lin)

    ;; Patch VCO with audio output of module
    ;; OUTPUT is a virtual module that represents the output sockets of the rack.
    (cl-synthesizer:add-patch voice "VCO" :saw "OUTPUT" :audio)
    
    voice))
  
(defun example ()
  "Two frequency modulated saw signals on left and right channel."
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       ;; Expose left and right channel line-out sockets
	       :output-sockets '(:left :right))))

    ;; Add saw-signal generators
    (cl-synthesizer:add-module
     rack "VOICE-1" #'make-voice :lfo-frequency 1.0 :vco-frequency 440.0)
    (cl-synthesizer:add-module
     rack "VOICE-2" #'make-voice :lfo-frequency 2.0 :vco-frequency 442.0)

    ;; Patch generators with left/right outputs
    (cl-synthesizer:add-patch rack "VOICE-1" :audio "OUTPUT" :left)
    (cl-synthesizer:add-patch rack "VOICE-2" :audio "OUTPUT" :right)

    ;; Write outputs to a Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("OUTPUT" :input-socket :left)
       ("OUTPUT" :input-socket :right))
     :filename "rack-example-1.wav")
    
    rack))

(defparameter *attach-audio* nil)
#|
;; Play rack for five seconds.
(cl-synthesizer:play-rack (example) 5 
    :attach-audio *attach-audio* :audio-output-sockets '(:left :right))
|#


