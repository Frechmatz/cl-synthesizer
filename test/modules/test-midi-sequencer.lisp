(in-package :cl-synthesizer-test)

(define-test test-midi-sequencer-1 ()
	     (let ((rack (cl-synthesizer:make-rack
			  :environment (cl-synthesizer:make-environment))))
	       (cl-synthesizer:add-module
		rack "MIDI-SEQUENCER"
		#'cl-synthesizer-modules-midi-sequencer:make-module :events
		(list 
		 (list :timestamp-milli-seconds 300
		       :midi-events (list
				     (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
		 (list :timestamp-milli-seconds 700
		       :midi-events (list
				     (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))))
	       (let ((midi-sequencer (cl-synthesizer:get-module rack "MIDI-SEQUENCER"))
		     (event-count 0))
		 (dotimes (i 44100)
		   (funcall (getf rack :update) nil)
		   (if (funcall (getf midi-sequencer :get-output) :midi-event)
		       (setf event-count (+ 1 event-count))))
		 (assert-equal 2 event-count))))

 
(define-test test-midi-sequencer-2 ()
	     (let ((rack (cl-synthesizer:make-rack
			  :environment (cl-synthesizer:make-environment))))
	       (cl-synthesizer:add-module
		rack "MIDI-SEQUENCER"
		#'cl-synthesizer-modules-midi-sequencer:make-module :events
		(list 
		 (list :timestamp-milli-seconds 3
		       :midi-events (list
				     (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
		 (list :timestamp-milli-seconds 4
		       :midi-events (list
				     (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))))
	       (let ((midi-sequencer (cl-synthesizer:get-module rack "MIDI-SEQUENCER"))
		     (event-count 0))
		 (dotimes (i 44100)
		   (funcall (getf rack :update) nil)
		   (if (funcall (getf midi-sequencer :get-output) :midi-event)
		       (setf event-count (+ 1 event-count))))
		 (assert-equal 2 event-count))))

 
