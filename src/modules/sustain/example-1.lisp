(defpackage :cl-synthesizer-modules-sustain-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-sustain-example-1)

(defun example ()
  "Sustain example"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    
    ;; Use MIDI sequencer for generation of Gate signals
    (cl-synthesizer:add-module
     rack "MIDI-SEQUENCER"
     #'cl-synthesizer-modules-midi-sequencer:make-module :events
     (list 
      (list :timestamp-milli-seconds 300
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-on-event
			   :channel 1
			   :note-number 69
			   :velocity 100)))
      (list :timestamp-milli-seconds 700
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-off-event
			   :channel 1
			   :note-number 69
			   :velocity 100)))
      (list :timestamp-milli-seconds 1800
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-on-event
			   :channel 1
			   :note-number 69
			   :velocity 100)))
      (list :timestamp-milli-seconds 2100
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-off-event
			   :channel 1
			   :note-number 69
			   :velocity 100)))))

    (cl-synthesizer:add-module
     rack "MIDI-IFC"
     #'cl-synthesizer-modules-midi-polyphonic-interface:make-module :voice-count 1)

    (cl-synthesizer:add-module
     rack "GATE-MULTIPLE"
     #'cl-synthesizer-modules-multiple:make-module :output-count 2)

    (cl-synthesizer:add-module
     rack "TRIGGER"
     #'cl-synthesizer-modules-trigger:make-module
     :trigger-threshold 4.9 :pulse-voltage 5.0)

    (cl-synthesizer:add-module
     rack "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 0.5 :v-peak 5.0)
    
    (cl-synthesizer:add-module
     rack "SUSTAIN"
     #'cl-synthesizer-modules-sustain:make-module)

    (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-IFC" :midi-events)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "GATE-MULTIPLE" :input)
    (cl-synthesizer:add-patch rack "GATE-MULTIPLE" :output-1 "TRIGGER" :input)
    (cl-synthesizer:add-patch rack "GATE-MULTIPLE" :output-2 "SUSTAIN" :gate)
    (cl-synthesizer:add-patch rack "TRIGGER" :output "SUSTAIN" :trigger)
    (cl-synthesizer:add-patch rack "VCO" :sine "SUSTAIN" :input)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-backend
     '(("MIDI-IFC" :output-socket :gate-1 :name "Gate")
       ("SUSTAIN" :input-socket :trigger :name "Sustain Trigger In")
       ("SUSTAIN" :input-socket :input :name "Sustain In")
       ("SUSTAIN" :output-socket :output :name "Sustain Out")
       ("SUSTAIN" :output-socket :done :name "Sustain Done Out"))
     :filename "cl-synthesizer-examples/sustain-example-1.csv")
    
    rack))

(defun run-example ()
  (let ((rack (example))) (cl-synthesizer:play-rack rack :duration-seconds 3)))

;; (run-example)

