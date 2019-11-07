;;
;;
;; A MIDI Keyboard Instrument
;;
;; Work in progress
;;

(defpackage :cl-synthesizer-playground-keyboard
  (:use :cl))

(in-package :cl-synthesizer-playground-keyboard)

(defun make-voice (name environment)
  (declare (ignore name))
  (let ((rack (cl-synthesizer:make-rack
	       :environment environment
	       :input-sockets '(:cv :gate)
	       :output-sockets '(:audio))))
    
    (cl-synthesizer:add-module
     rack "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency (cl-synthesizer-midi:get-note-number-frequency 0)
     :v-peak 5.0)

    (cl-synthesizer:add-module
     rack "ADSR"
     #'cl-synthesizer-modules-adsr:make-module
     :attack-time-ms 50 :attack-target-output 5.0
     :decay-time-ms 20 :decay-target-output 3.0
     :release-time-ms 30
     :backward-coupled t)
    
    (cl-synthesizer:add-module rack "VCA" #'cl-synthesizer-modules-vca:make-module :cv-max 5.0 :exponential nil)

    (cl-synthesizer:add-patch rack "VCO" :sine "VCA" :input)
    (cl-synthesizer:add-patch rack "ADSR" :cv "VCA" :cv)
    (cl-synthesizer:add-patch rack "INPUT" :cv "VCO" :cv-exp)
    (cl-synthesizer:add-patch rack "INPUT" :gate "ADSR" :gate)
    (cl-synthesizer:add-patch rack "VCA" :output "OUTPUT" :audio)
    
    rack))

(defun keyboard ()
  "Keyboard"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :output-sockets '(:line-out-1)
	       :input-sockets '(:midi-events)
	       )))
    ;; Add voices
    (cl-synthesizer:add-module rack "VOICE-1" #'make-voice)
    (cl-synthesizer:add-module rack "VOICE-2" #'make-voice)
    (cl-synthesizer:add-module
     rack "MIDI-IFC" #'cl-synthesizer-modules-midi-polyphonic-interface:make-module :voice-count 2)
    (cl-synthesizer:add-patch rack "INPUT" :midi-events "MIDI-IFC" :midi-events)

    ;; Connect voices with midi-interface
    (cl-synthesizer:add-patch rack "MIDI-IFC" :cv-1 "VOICE-1" :cv)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "VOICE-1" :gate)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :cv-2 "VOICE-2" :cv)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-2 "VOICE-2" :gate)

    (cl-synthesizer:add-module rack "MIXER" #'cl-synthesizer-modules-mixer:make-module
			       :channel-count 2
			       :channel-cv-max 5.0
			       :channel-cv-gain 5.0
			       :main-cv-max 5.0
			       :main-cv-gain 2.5)

    (cl-synthesizer:add-patch rack "VOICE-1" :audio "MIXER" :channel-1)
    (cl-synthesizer:add-patch rack "VOICE-2" :audio "MIXER" :channel-2)
    (cl-synthesizer:add-patch rack "MIXER" :output "OUTPUT" :line-out-1)

    ;; Write LINE-OUT to Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("OUTPUT" :input-socket :line-out-1))
     :filename "keyboard.wav")
    
    rack))

#|
(let ((rack (keyboard)))
  (time (cl-synthesizer-experimental::play-rack rack :duration-seconds 15 
      :attach-audio t :audio-output-sockets '(:line-out-1) 
      :attach-midi t :midi-input-socket :midi-events)))
|#

