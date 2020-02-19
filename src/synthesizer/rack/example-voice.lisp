(defpackage :cl-synthesizer-rack-example-voice
  (:documentation "Example: Nested Racks.")
  (:use :cl))

(in-package :cl-synthesizer-rack-example-voice)

(defun make-voice (name environment &key lfo-frequency vco-frequency)
  "Factory function of the voice."
  (declare (ignore name))
  ;; The voice makes use of already available modules, so let it be a rack.
  (let ((voice
	 (cl-synthesizer:make-rack
	  :environment environment
	  ;; Expose an :audio output socket
	  :output-sockets '(:audio))))
    ;;
    ;; Add LFO and VCO and patch them to create a frequency modulated saw wave
    ;;
    (cl-synthesizer:add-module
     voice "LFO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency lfo-frequency :v-peak 5.0)

    (cl-synthesizer:add-module
     voice "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency vco-frequency :v-peak 5.0 :cv-lin-hz-v 20.0)

    (cl-synthesizer:add-patch voice "LFO" :sine "VCO" :cv-lin)
    ;; Patch the VCO saw output with the :audio socket that is exposed by the voice
    (cl-synthesizer:add-patch voice "VCO" :saw "OUTPUT" :audio)
    
    voice))
  
(defun example ()
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       ;; Expose line-out sockets
	       :output-sockets '(:left :right))))

    (cl-synthesizer:add-module
     rack "VOICE-1" #'make-voice :lfo-frequency 1.0 :vco-frequency 440.0)
    (cl-synthesizer:add-module
     rack "VOICE-2" #'make-voice :lfo-frequency 2.0 :vco-frequency 442.0)

    (cl-synthesizer:add-patch rack "VOICE-1" :audio "OUTPUT" :left)
    (cl-synthesizer:add-patch rack "VOICE-2" :audio "OUTPUT" :right)

    ;; Let a monitor write the Wave file.
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-backend
     '(("OUTPUT" :input-socket :left)
       ("OUTPUT" :input-socket :right))
     :filename "cl-synthesizer-examples/rack-example-voice.wav")
    
    rack))

(defun run-example ()
  (cl-synthesizer:play-rack (example) :duration-seconds 5))
  
;;(run-example)

