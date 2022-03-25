(defpackage :cl-synthesizer-modules-midi-relative-cc-interface-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-midi-relative-cc-interface-example-1)

(defun example ()
  "MIDI Relative CC-Interface Example"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))

    (cl-synthesizer:add-module
     rack "MIDI-CC-IFC" #'cl-synthesizer-modules-midi-relative-cc-interface:make-module
     :mappings '((:controller-number 112 :control-value 61 :offset -1.0)
		 (:controller-number 112 :control-value 67 :offset 1.0))
     :initial-output 2.5
     :channel nil)

    (cl-synthesizer:add-module
     rack "MIDI-SEQUENCER"
     #'cl-synthesizer-modules-midi-sequencer:make-module :events
     (list 
      (list :timestamp-milli-seconds 100
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-control-change-event :channel 1 :controller-number 112 :control-value 61)))
      (list :timestamp-milli-seconds 200
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-control-change-event :channel 1 :controller-number 112 :control-value 61)))
      (list :timestamp-milli-seconds 300
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-control-change-event :channel 1 :controller-number 112 :control-value 61)))
      (list :timestamp-milli-seconds 400
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-control-change-event :channel 1 :controller-number 112 :control-value 61)))
      (list :timestamp-milli-seconds 500
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-control-change-event :channel 1 :controller-number 112 :control-value 67)))
      (list :timestamp-milli-seconds 600
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-control-change-event :channel 1 :controller-number 112 :control-value 67)))
      (list :timestamp-milli-seconds 700
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-control-change-event :channel 1 :controller-number 112 :control-value 67)))
      (list :timestamp-milli-seconds 800
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-control-change-event :channel 1 :controller-number 112 :control-value 67)))))
    
    (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-CC-IFC" :midi-events)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-file-agent:make-backend
     '(("MIDI-CC-IFC" :output-socket :output :name "CC-Out"))
     :filename "cl-synthesizer-examples/midi-relative-cc-interface-example-1.csv")

    rack))

(defun run-example ()
  (cl-synthesizer::play-rack (example) :duration-seconds 0.9))

;; (run-example)
