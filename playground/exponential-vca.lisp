;;
;;
;; Exponential VCA Playground
;;
;;

(defpackage :cl-synthesizer-playground-exp-vca
  (:use :cl))

(in-package :cl-synthesizer-playground-exp-vca)

(defparameter *exp-vca-adsr-environment* (cl-synthesizer:make-environment))

(defun synthesizer-playground-exponential-vca ()
  "Exponential VCA example"
  (let ((rack (cl-synthesizer:make-rack :environment *exp-vca-adsr-environment*)))

    ;; Set up MIDI Interface and connect it to the MIDI input of the Rack
    (cl-synthesizer:add-module rack "MIDI-IFC"
				#'cl-synthesizer-modules-midi-interface:midi-interface :voice-count 1)
    (cl-synthesizer:add-patch rack "MIDI-IN" :midi-events "MIDI-IFC" :midi-events)

    ;; Setup ADSR
    (cl-synthesizer:add-module rack "ADSR" #'cl-synthesizer-modules-envelope:envelope
			       :segments '((:duration-ms 1000 :target-cv 5 :required-gate-state :on)
					   (:duration-ms 1000 :target-cv 3 :required-gate-state :on)
					   (:required-gate-state :on)
					   (:duration-ms 1000 :target-cv 0 :required-gate-state :off)))
    (cl-synthesizer:add-module rack "ADSR-CV-MULTIPLE" #'cl-synthesizer-modules-multiple:multiple :output-count 2)
    (cl-synthesizer:add-patch rack "ADSR" :cv "ADSR-CV-MULTIPLE" :input)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "ADSR" :gate)
    
    (cl-synthesizer:add-module rack "VCA-LIN" #'cl-synthesizer-modules-vca:vca
			       :input-min -5.0
			       :input-max 5.0
			       :output-min -5.0
			       :output-max 5.0)
    (cl-synthesizer:add-module rack "VCA-EXP" #'cl-synthesizer-modules-vca:vca-exponential
			       :cv-max 5.0)
    (cl-synthesizer:add-patch rack "ADSR-CV-MULTIPLE" :output-1 "VCA-LIN" :cv)
    (cl-synthesizer:add-patch rack "ADSR-CV-MULTIPLE" :output-2 "VCA-EXP" :cv)

    (cl-synthesizer:add-module rack "AUDIO-SIGNAL" #'cl-synthesizer-modules-fixed-output:fixed-output :value 4.9)
    (cl-synthesizer:add-module rack "AUDIO-SIGNAL-MULTIPLE" #'cl-synthesizer-modules-multiple:multiple :output-count 2)
    (cl-synthesizer:add-patch rack "AUDIO-SIGNAL" :out "AUDIO-SIGNAL-MULTIPLE" :input)
    (cl-synthesizer:add-patch rack "AUDIO-SIGNAL-MULTIPLE" :output-1 "VCA-LIN" :input)
    (cl-synthesizer:add-patch rack "AUDIO-SIGNAL-MULTIPLE" :output-2 "VCA-EXP" :input)
    
    ;; Add monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "ADSR-CV-MULTIPLE" :input-socket :input)
       (:channel-2 "AUDIO-SIGNAL" :output-socket :out)
       ;; Output of VCA-LIN must exactly follow ADSR output
       (:channel-3 "VCA-LIN" :output-socket :output)
       (:channel-4 "VCA-EXP" :input-socket :cv)
       (:channel-5 "VCA-EXP" :output-socket :output)

       )
     :filename "/Users/olli/waves/vcaplayground.wav")

    rack))

(defun play ()
  (cl-synthesizer-util:play-rack
   (synthesizer-playground-exponential-vca)
   10 
   ;;:attach-speaker t
   :midi-device
   (cl-synthesizer-device-midi-sequencer:midi-sequencer
    "Midi-Device"
    *exp-vca-adsr-environment*
    :events
    (list 
     (list :timestamp-milli-seconds 20
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
     (list :timestamp-milli-seconds 2020
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))

     ))))

;; (play)
