;;
;;
;; Envelope Playground
;;
;;

(defpackage :cl-synthesizer-playground-envelope
  (:use :cl))

(in-package :cl-synthesizer-playground-envelope)

(defparameter *adsr-environment* (cl-synthesizer:make-environment))

(defun synthesizer-playground-adsr ()
  "Envelope-Example"
  (let ((rack (cl-synthesizer:create-rack :environment *adsr-environment*)))

    ;; Set up MIDI Interface and connect it to the MIDI input of the Rack
    (cl-synthesizer:add-module rack "MIDI-IFC"
				#'cl-synthesizer-modules-midi-interface:midi-interface :voice-count 1)
    (cl-synthesizer:add-patch rack "MIDI-IN" :midi-events "MIDI-IFC" :midi-events)

    ;; Add VCO, ADSR, VCA
    (cl-synthesizer:add-module rack "VCO" #'cl-synthesizer-modules-vco:vco
				:base-frequency (cl-synthesizer-midi:get-note-number-frequency 0)
				:cv-max 5
				:f-max 13000
				:v-peak 5)
    (cl-synthesizer:add-module rack "ADSR" #'cl-synthesizer-modules-envelope:envelope
			       :segments '((:name "Attack" :time-ms 1000 :target-cv 5 :requires-gate t)
					   (:name "Decay" :time-ms 1000 :target-cv 3 :requires-gate t)
					   (:name "Sustain" :requires-gate t)
					   (:name "Release" :time-ms 1000 :target-cv 0 :requires-gate nil)))
    (cl-synthesizer:add-module rack "VCA" #'cl-synthesizer-modules-vca:vca)

    (cl-synthesizer:add-patch rack "MIDI-IFC" :cv-1 "VCO" :cv)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "ADSR" :gate)
    (cl-synthesizer:add-patch rack "VCO" :sine "VCA" :input)
    (cl-synthesizer:add-patch rack "VCA" :out "LINE-OUT" :channel-1)
    (cl-synthesizer:add-patch rack "ADSR" :cv "VCA" :cv)

    ;; Add ADSR-Output and VCA-Output monitor
    (cl-synthesizer:register-monitor
     rack
     "ADSR-VCA"
     #'cl-synthesizer-monitor:wave-file-handler
     '((:channel-1 "VCA" :output-socket :out) (:channel-2 "ADSR" :output-socket :cv))
     :filename "/Users/olli/waves/adsrplayground.wav")

    ;; Add LINE-OUT Monitor
    (cl-synthesizer:register-monitor
     rack
     "LINE-OUT"
     #'cl-synthesizer-monitor:wave-file-handler
     '((:channel-1 "LINE-OUT" :input-socket :channel-1))
     :filename "/Users/olli/waves/lineout.wav")
    
    rack))

(defun play ()
  (cl-synthesizer-util:play-rack
   (synthesizer-playground-adsr)
   10 
   :attach-speaker t
   :midi-device
   (cl-synthesizer-device-midi-sequencer:midi-sequencer
    "Midi-Device"
    *adsr-environment*
    :events
    (list 
     (list :timestamp-milli-seconds 20
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
     (list :timestamp-milli-seconds 2020
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))

     (list :timestamp-milli-seconds 3020
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
     (list :timestamp-milli-seconds 3520
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))
     ))))

;; (play)
