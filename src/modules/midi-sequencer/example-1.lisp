(defpackage :cl-synthesizer-modules-midi-sequencer-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-midi-sequencer-example-1)

(defparameter *attach-speaker* t)

(defun example ()
  "Midi-Sequencer example"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))

    ;; Add sequencer
    (cl-synthesizer:add-module
     rack
     "MIDI-SEQUENCER"
     #'cl-synthesizer-modules-midi-sequencer:midi-sequencer :events
     (list 
      (list :timestamp-milli-seconds 0
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
      (list :timestamp-milli-seconds 1000
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))
      (list :timestamp-milli-seconds 2000
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-on-event 1 75 100)))
      (list :timestamp-milli-seconds 2500
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-off-event 1 75 100)))))

    ;; Add MIDI Interface and connect it with the MIDI Sequencer
    (cl-synthesizer:add-module
     rack
     "MIDI-IFC"
     #'cl-synthesizer-modules-midi-interface:midi-interface :voice-count 1)
    (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-IFC" :midi-events)

    ;; Add VCO
    (cl-synthesizer:add-module
     rack "VCO" #'cl-synthesizer-modules-vco:vco-exponential
     :base-frequency (cl-synthesizer-midi:get-note-number-frequency 0)
     :f-max 12000
     :v-peak 5)

    ;; Add ADSR
    (cl-synthesizer:add-module
     rack "ADSR"
     #'cl-synthesizer-modules-envelope:envelope
     :segments
     '(;; Attack
       (:duration-ms 100 :target-cv 5 :required-gate-state :on)
       ;; Decay
       (:duration-ms 50 :target-cv 3 :required-gate-state :on)
       ;; Sustain
       (:required-gate-state :on)
       ;; Release
       (:duration-ms 100 :target-cv 0 :required-gate-state :off)))
    
    ;; Add VCA
    (cl-synthesizer:add-module rack "VCA" #'cl-synthesizer-modules-vca:vca :cv-max 5.0)

    ;; Connect VCA with ADSR and VCO
    (cl-synthesizer:add-patch rack "VCA" :output-linear "LINE-OUT" :channel-1)
    (cl-synthesizer:add-patch rack "ADSR" :cv "VCA" :cv)
    (cl-synthesizer:add-patch rack "VCO" :triangle "VCA" :input)
    
    ;; Connect Midi interface with ADSR and VCO
    (cl-synthesizer:add-patch rack "MIDI-IFC" :cv-1 "VCO" :cv)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "ADSR" :gate)

    ;; Record LINE-OUT into a wave file
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "LINE-OUT" :input-socket :channel-1))
     :filename "waves/midi-sequencer-example-1.wav")
    
    rack))

;;(cl-synthesizer:play-rack (example) 5 :attach-speaker *attach-speaker*)
