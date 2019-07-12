(defpackage :cl-synthesizer-modules-adsr-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-adsr-example-1)

(defun make-voice (name environment &key exponential)
  (let ((rack (cl-synthesizer:make-rack
	       :environment environment
	       :input-sockets '(:gate)
	       :output-sockets '(:adsr-out))))

    (cl-synthesizer:add-module
     rack "ADSR"
     #'cl-synthesizer-modules-adsr:make-module
     :attack-time-ms 500 :attack-target-output 5.0
     :decay-time-ms 250 :decay-target-output -3.0
     :release-time-ms 1000
     :exponential exponential)

    (cl-synthesizer:add-patch rack "INPUT" :gate "ADSR" :gate)
    (cl-synthesizer:add-patch rack "ADSR" :cv "OUTPUT" :adsr-out)

    rack))

(defun example ()
  "ADSR example. Linear vs. Exponential"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module
     rack "MIDI-SEQUENCER"
     #'cl-synthesizer-modules-midi-sequencer:make-module :events
     (list 
      (list :timestamp-milli-seconds 0
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
      (list :timestamp-milli-seconds 1500
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))))

    (cl-synthesizer:add-module
     rack "MIDI-IFC"
     #'cl-synthesizer-modules-midi-interface:make-module :voice-count 1)

    (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-IFC" :midi-events)
    
    (cl-synthesizer:add-module
     rack "GATE-MULTIPLE"
     #'cl-synthesizer-modules-multiple:make-module
     :output-count 2)

    (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "GATE-MULTIPLE" :input)

    (cl-synthesizer:add-module
     rack "LINEAR" #'make-voice :exponential nil)
    (cl-synthesizer:add-patch rack "GATE-MULTIPLE" :output-1 "LINEAR" :gate)
    
    (cl-synthesizer:add-module
     rack "EXPONENTIAL" #'make-voice :exponential t)
    (cl-synthesizer:add-patch rack "GATE-MULTIPLE" :output-2 "EXPONENTIAL" :gate)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("GATE-MULTIPLE" :input-socket :input :name "Gate")
       ("LINEAR" :output-socket :adsr-out :name "ADSR Linear")
       ("EXPONENTIAL" :output-socket :adsr-out :name "ADSR Exponential"))
     :filename "cl-synthesizer-examples/adsr-example-1.csv")
    
    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer:play-rack rack :duration-seconds 3)))

;; (run-example)
