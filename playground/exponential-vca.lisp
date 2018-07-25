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
    
    ;; Set up VCA that will take care of the characteristic of the ADSR envelope
    (cl-synthesizer:add-module rack "VCA-ADSR-CV" #'cl-synthesizer-modules-vca::vca-ng
			       :max-amplification 1.0
			       :max-amplification-cv 2.5)
    ;;; Set amplification fixed to 5.0. Amplification shall be 1.0
    (cl-synthesizer:add-module rack "VCA-ADSR-AMPLIFICATION-INPUT-CV" #'cl-synthesizer-modules-fixed-output:fixed-output :value 2.5)
    (cl-synthesizer:add-patch rack "VCA-ADSR-AMPLIFICATION-INPUT-CV" :out "VCA-ADSR-CV" :cv)

    ;; Connect ADSR Gate input with Gate output of MIDI-IFC
    (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "ADSR" :gate)
    ;; Connect ADSR CV output with VCA input
    (cl-synthesizer:add-patch rack "ADSR" :cv "VCA-ADSR-CV" :input)
    
    ;; Add monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "ADSR" :output-socket :cv)
       (:channel-2 "VCA-ADSR-AMPLIFICATION-INPUT-CV" :output-socket :out)
       ;; for now both outputs should exactly follow the CV output of ADSR
       (:channel-3 "VCA-ADSR-CV" :output-socket :output-linear)
       (:channel-4 "VCA-ADSR-CV" :output-socket :output-exponential)
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
