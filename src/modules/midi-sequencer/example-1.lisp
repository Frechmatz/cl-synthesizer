(defpackage :cl-synthesizer-modules-midi-sequencer-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-midi-sequencer-example-1)

(defun example ()
  "Midi-Sequencer example"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :output-sockets '(:line-out))))

    ;; Add sequencer
    (cl-synthesizer:add-module
     rack
     "MIDI-SEQUENCER"
     #'cl-synthesizer-modules-midi-sequencer:make-module :events
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
     #'cl-synthesizer-modules-midi-interface:make-module :voice-count 1)
    (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-IFC" :midi-events)

    ;; Add VCO
    (cl-synthesizer:add-module
     rack "VCO" #'cl-synthesizer-modules-vco:make-module
     :base-frequency (cl-synthesizer-midi:get-note-number-frequency 0)
     :v-peak 5.0)

    ;; Add ADSR
    (cl-synthesizer:add-module
     rack "ADSR"
     #'cl-synthesizer-modules-adsr:make-module
     :attack-time-ms 100 :attack-target-output 5.0
     :decay-time-ms 50 :decay-target-output 3.0
     :release-time-ms 100)
    
    ;; Add VCA
    (cl-synthesizer:add-module rack "VCA" #'cl-synthesizer-modules-vca:make-module :cv-max 5.0)

    ;; Connect VCA with ADSR and VCO
    (cl-synthesizer:add-patch rack "VCA" :output-linear "OUTPUT" :line-out)
    (cl-synthesizer:add-patch rack "ADSR" :cv "VCA" :cv)
    (cl-synthesizer:add-patch rack "VCO" :triangle "VCA" :input)
    
    ;; Connect Midi interface with ADSR and VCO
    (cl-synthesizer:add-patch rack "MIDI-IFC" :cv-1 "VCO" :cv-exp)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "ADSR" :gate)

    ;; Record LINE-OUT into a wave file
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("OUTPUT" :input-socket :line-out))
     :filename "cl-synthesizer-examples/midi-sequencer-example-1.wav")
    
    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer::play-rack rack :duration-seconds 5)))

;; (run-example)

