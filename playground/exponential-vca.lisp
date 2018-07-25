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
			       :segments '(;; Attack
					   (:duration-ms 1000 :target-cv 5 :required-gate-state :on)
					   ;; Decay
					   (:duration-ms 500 :target-cv 2.5 :required-gate-state :on)
					   ;; Sustain
					   (:required-gate-state :on)
					   ;; Release
					   (:duration-ms 1000 :target-cv 0 :required-gate-state :off)))
    ;; Connect ADSR Gate input with Gate output of MIDI-IFC
    (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "ADSR" :gate)
    ;; Dispatch output of ADSR to a multiple
    (cl-synthesizer:add-module rack "ADSR-CV-MULTIPLE" #'cl-synthesizer-modules-multiple:multiple
			       :output-count 2)
    (cl-synthesizer:add-patch rack "ADSR" :cv "ADSR-CV-MULTIPLE" :input)

    ;; Set up VCA which will follow the CV of ADSR with linear characteristic
    ;; this is realized with a fixed input voltage of the CV input of the VCA
    (cl-synthesizer:add-module rack "VCA-LINEAR" #'cl-synthesizer-modules-vca::vca-ng
			       :max-amplification 1.0
			       :max-amplification-cv 2.5)
    (cl-synthesizer:add-module rack "VCA-LINEAR-CONST-CV" #'cl-synthesizer-modules-fixed-output:fixed-output :value 2.5)
    (cl-synthesizer:add-patch rack "VCA-LINEAR-CONST-CV" :out "VCA-LINEAR" :cv)
    (cl-synthesizer:add-patch rack "ADSR-CV-MULTIPLE" :output-1 "VCA-LINEAR" :input)

    ;; Set up VCA which will follow the cv of ADSR with exponential characteristic
    ;; this is realized with a fixed input voltage of the VCA and the CV input
    ;; of the VCA being modulated by the CV of the ADSR
    (cl-synthesizer:add-module rack "VCA-EXPONENTIAL" #'cl-synthesizer-modules-vca::vca-ng
			       :max-amplification 1.0
			       :max-amplification-cv 5.0) ;; max output of ADSR CV
    (cl-synthesizer:add-module rack "VCA-EXPONENTIAL-CONST-INPUT" #'cl-synthesizer-modules-fixed-output:fixed-output :value 5.0)
    (cl-synthesizer:add-patch rack "VCA-EXPONENTIAL-CONST-INPUT" :out "VCA-EXPONENTIAL" :input)
    (cl-synthesizer:add-patch rack "ADSR-CV-MULTIPLE" :output-2 "VCA-EXPONENTIAL" :cv)

    ;; Add monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "ADSR" :output-socket :cv)
       (:channel-2 "VCA-LINEAR" :output-socket :output-linear)
       (:channel-3 "VCA-EXPONENTIAL" :output-socket :output-exponential)
       )
     :filename "/Users/olli/waves/vcaplayground.wav")

    rack))

(defun play ()
  (cl-synthesizer-util:play-rack
   (synthesizer-playground-exponential-vca)
   5
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
