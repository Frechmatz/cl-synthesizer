(defpackage :cl-synthesizer-modules-adsr-example-2
  (:use :cl))

(in-package :cl-synthesizer-modules-adsr-example-2)

(defun example ()
  "ADSR case study. Abort attack by setting gate to off."
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
			  (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))))

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
     '(("ADSR" :input-socket :gate :name "ADSR Gate In" :format "~,5F")
       ("ADSR" :output-socket :cv :name "ADSR Out" :format "~,5F"))
     :filename "waves/adsr-example-2.csv")
    
    rack))

;;(cl-synthesizer:play-rack (example) 3)