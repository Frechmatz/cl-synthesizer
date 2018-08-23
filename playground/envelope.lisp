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
  (let ((rack (cl-synthesizer:make-rack :environment *adsr-environment*)))

    ;; Set up MIDI Interface and connect it to the MIDI input of the Rack
    (cl-synthesizer:add-module rack "MIDI-IFC"
				#'cl-synthesizer-modules-midi-interface:midi-interface :voice-count 1)
    (cl-synthesizer:add-patch rack "MIDI-IN" :midi-events "MIDI-IFC" :midi-events)

    ;; Add VCO, ADSR, VCA
    (cl-synthesizer:add-module rack "VCO" #'cl-synthesizer-modules-vco:vco-exponential
				:base-frequency (cl-synthesizer-midi:get-note-number-frequency 0)
				:f-max 13000
				:v-peak 5)
    (cl-synthesizer:add-module rack "ADSR" #'cl-synthesizer-modules-envelope:envelope
			       :segments '((:duration-ms 1000 :target-cv 5 :required-gate-state :on
					    :duration-controller
					    (:socket :attack-duration
					     :input-min -5.0
					     :input-max 5.0
					     :output-min -5000
					     :output-max 5000))
					   (:duration-ms 1000 :target-cv 3 :required-gate-state :on)
					   (:required-gate-state :on)
					   (:duration-ms 1000 :target-cv 0 :required-gate-state :off)))
    (cl-synthesizer:add-module rack "VCA" #'cl-synthesizer-modules-vca:vca
			       :cv-max 5.0)

    ;; ADSR attack duration controller
    (cl-synthesizer:add-module rack "ADSR-ATTACK-DURATION-CTRL" #'cl-synthesizer-modules-fixed-output:fixed-output :value 0)
    (cl-synthesizer:add-patch rack "ADSR-ATTACK-DURATION-CTRL" :out "ADSR" :attack-duration)
    
    (cl-synthesizer:add-patch rack "MIDI-IFC" :cv-1 "VCO" :cv)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "ADSR" :gate)
    (cl-synthesizer:add-patch rack "VCO" :sine "VCA" :input)
    (cl-synthesizer:add-patch rack "VCA" :output-linear "LINE-OUT" :channel-1)
    (cl-synthesizer:add-patch rack "ADSR" :cv "VCA" :cv)

    ;; Add ADSR-Output and VCA-Output monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "VCA" :output-socket :output-linear) (:channel-2 "ADSR" :output-socket :cv))
     :filename "/Users/olli/waves/adsrplayground.wav")

    ;; Add LINE-OUT Monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
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

(defun play2 ()
  (cl-synthesizer-util:play-rack
   (synthesizer-playground-adsr)
   7 
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
     (list :timestamp-milli-seconds 4020
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))

     ))))

;; (play2)


(defun play3 ()
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
     (list :timestamp-milli-seconds 700
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))

     ))))

;; (play3)

