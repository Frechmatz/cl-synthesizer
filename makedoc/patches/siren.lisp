(defpackage :cl-synthesizer-patches-siren
  (:use :cl)
  (:documentation "Two frequency modulated saws"))

(in-package :cl-synthesizer-patches-siren)

(defun make-voice (name environment &key lfo-frequency vco-frequency)
  "Frequency modulated saw"
  (declare (ignore name))
  (let ((voice
	  (cl-synthesizer:make-rack
	   :environment environment
	   :output-sockets '(:audio))))
    
    (cl-synthesizer:add-module
     voice "LFO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency lfo-frequency :v-peak 5.0)

    (cl-synthesizer:add-module
     voice "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency vco-frequency :v-peak 5.0 :cv-lin-hz-v 20.0)

    (cl-synthesizer:add-patch voice "LFO" :sine "VCO" :cv-lin)
    (cl-synthesizer:add-patch voice "VCO" :saw "OUTPUT" :audio)
    
    voice))

(defun example ()
  (let ((rack (cl-synthesizer:make-rack
               :environment (cl-synthesizer:make-environment))))

    (cl-synthesizer:add-module
     rack "VOICE-1" #'make-voice :lfo-frequency 1.0 :vco-frequency 440.0)
    (cl-synthesizer:add-module
     rack "VOICE-2" #'make-voice :lfo-frequency 2.0 :vco-frequency 442.0)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-file-agent:make-backend
     '(("VOICE-1" :output-socket :audio)
       ("VOICE-2" :output-socket :audio))
     :filename "docs/siren.wav"
     :v-peak 5.0)
    
    rack))

(defun run-example ()
  (cl-synthesizer:play-rack (example) :duration-seconds 2.0))

;;(run-example)
