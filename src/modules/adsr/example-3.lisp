(defpackage :cl-synthesizer-modules-adsr-example-3
  (:use :cl))

(in-package :cl-synthesizer-modules-adsr-example-3)

(defun example ()
  "ADSR case study. Abort attack and then retrigger during release phase."
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module
     rack "MIDI-SEQUENCER"
     #'cl-synthesizer-modules-midi-sequencer:make-module :events
     (list 
      (list :timestamp-milli-seconds 0
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
      (list :timestamp-milli-seconds 300
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))
      (list :timestamp-milli-seconds 800
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
      (list :timestamp-milli-seconds 1200
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))
      ))

    (cl-synthesizer:add-module
     rack "MIDI-IFC"
     #'cl-synthesizer-modules-midi-interface:make-module :voice-count 1)

    (cl-synthesizer:add-module
     rack "ADSR"
     #'cl-synthesizer-modules-adsr:make-module
     :attack-time-ms 500 :attack-target-output 5.0
     :decay-time-ms 250 :decay-target-output 4.0
     :release-time-ms 1000)
    
    (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-IFC" :midi-events)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "ADSR" :gate)
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("ADSR" :input-socket :gate :name "ADSR Gate In")
       ("ADSR" :output-socket :cv :name "ADSR Out"))
     :filename "cl-synthesizer-examples/adsr-example-3.csv")
    
    rack))

(defun run-example ()
  (cl-synthesizer:play-rack (example) :duration-seconds 3))

;; (run-example)
