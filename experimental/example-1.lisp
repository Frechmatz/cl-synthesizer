(defpackage :cl-synthesizer-experimental-example-1
  (:use :cl))

(in-package :cl-synthesizer-experimental-example-1)

(defun make-voice (name environment &key lfo-frequency vco-frequency)
  "Frequency modulated saw"
  (declare (ignore name))
  (let ((voice
	 (cl-synthesizer:make-rack
	  :environment environment
	  ;; Expose audio output socket
	  :output-sockets '(:audio))))
    
    (cl-synthesizer:add-module
     voice "LFO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency lfo-frequency :v-peak 0.1 :f-max 500.0 :cv-max 5.0)

    (cl-synthesizer:add-module
     voice "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency vco-frequency :f-max 5000.0 :v-peak 5.0 :cv-max 5.0)

    (cl-synthesizer:add-patch voice "LFO" :sine "VCO" :cv-lin)
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

    rack))

(defun run-example ()
  (cl-synthesizer-experimental:play-rack
   (example)
   :duration-seconds 5
   :attach-audio t
   :audio-output-sockets '(:left :right)))
  
;;(run-example)

