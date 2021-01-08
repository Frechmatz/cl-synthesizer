(defpackage :cl-synthesizer-profiling-midi-sequencer
  (:use :cl))

(in-package :cl-synthesizer-profiling-midi-sequencer)

(defun make-test-rack ()
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :input-sockets '(:input))))

    (cl-synthesizer:add-module
     rack
     "MIDI-SEQUENCER"
     #'cl-synthesizer-modules-midi-sequencer:make-module :events
     (list 
      (list :timestamp-milli-seconds 0
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-on-event :channel 1 :note-number 69 :velocity 100)))
      (list :timestamp-milli-seconds 1000
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-off-event :channel 1 :note-number 69 :velocity 100)))
      (list :timestamp-milli-seconds 2000
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-on-event :channel 1 :note-number 75 :velocity 100)))
      (list :timestamp-milli-seconds 2500
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-off-event :channel 1 :note-number 75 :velocity 100)))))
     
  rack))

