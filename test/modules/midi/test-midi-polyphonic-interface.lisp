(in-package :cl-synthesizer-test)



;;
;; Midi-Interface
;;

(defun test-polyphonic-midi-interface-make-midi-interface
    (voice-count
     &key
       (channel nil))
  (cl-synthesizer-modules-midi-polyphonic-interface:make-module
   "Test-Midi-Interface"
   (cl-synthesizer:make-environment)
   :voice-count voice-count
   :channel channel
   :cv-velocity-max 1270.0))

(defun run-polyphonic-test-case-midi-ifc (test-case)
  (let ((ifc (test-polyphonic-midi-interface-make-midi-interface
	      (getf test-case :voice-count)
	      :channel (getf test-case :channel))))
    (dolist (cur-test-case (getf test-case :test-cases))
      (update-module ifc (list (list :midi-events (getf cur-test-case :events))))
      (dolist (cur-output (getf cur-test-case :outputs))
	(assert-equal (float (second cur-output)) (float (get-module-output ifc (first cur-output))))))))

(define-test test-polyphonic-midi-interface-1 ()
	     (let ((test
		    '(:voice-count 2 
		      :test-cases
		      ((:events nil :outputs ((:CV-1 0) (:CV-2 0) (:GATE-1 0) (:GATE-2 0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

(define-test test-polyphonic-midi-interface-2 ()
	     (let ((test
		    `(:voice-count 2 
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 60
						:velocity 0))
					     :outputs ((:CV-1 5.0)
						       (:GATE-1 5.0)
						       (:CV-2 0)
						       (:GATE-2 0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))


(define-test test-polyphonic-midi-interface-3 ()
	     (let ((test
		    `(:voice-count 2 
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 60
						:velocity 0))
					     :outputs ((:CV-1 5.0)
						       (:GATE-1 5.0)
						       (:CV-2 0)
						       (:GATE-2 0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 36
						:velocity 0))
					     :outputs ((:CV-1 5.0)
						       (:GATE-1 5.0)
						       (:CV-2 3.0)
						       (:GATE-2 5.0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))





;;
;; tests for update with multiple midi-events
;;
(define-test test-polyphonic-midi-interface-multiple-events-1 ()
	     (let ((test
		    `(:voice-count 2 
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 60
						:velocity 0)
					       ,(cl-synthesizer-midi-event:make-note-off-event
						 :channel 1
						 :note-number 60
						 :velocity 0))
					     :outputs ((:CV-1 5.0)
						       (:GATE-1 0)
						       (:CV-2 0)
						       (:GATE-2 0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

(define-test test-polyphonic-midi-interface-multiple-events-2 ()
	     (let ((test
		    `(:voice-count 2 
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 60
						:velocity 0)
					       ,(cl-synthesizer-midi-event:make-note-on-event
						 :channel 1
						 :note-number 36
						 :velocity 0))
					     :outputs ((:CV-1 5.0)
						       (:GATE-1 5.0)
						       (:CV-2 3.0)
						       (:GATE-2 5.0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

(define-test test-polyphonic-midi-interface-multiple-events-3 ()
	     (let ((test
		    `(:voice-count 2 
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 60
						:velocity 0)
					       ,(cl-synthesizer-midi-event:make-note-on-event
						 :channel 1
						 :note-number 36
						 :velocity 0)
					       ,(cl-synthesizer-midi-event:make-note-off-event
						 :channel 1
						 :note-number 60
						 :velocity 0))
					     :outputs ((:CV-1 5.0)
						       (:GATE-1 0)
						       (:CV-2 3.0)
						       (:GATE-2 5.0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

;; Test channel support

(define-test test-polyphonic-midi-interface-channel-1 ()
	     (let ((test
		    `(:voice-count 1 :channel 2
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 64
						:velocity 0))
					     :outputs ((:CV-1 0)
						       (:GATE-1 0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

(define-test test-polyphonic-midi-interface-channel-2 ()
	     (let ((test
		    `(:voice-count 1 :channel 1
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 60
						:velocity 0))
					     :outputs ((:CV-1 5.0)
						       (:GATE-1 5.0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

;;
;; Tests that check that gate goes down for one tick when a voice
;; is stolen in polyphonic mode
;;

(define-test test-polyphonic-midi-interface-gate-retrigger-1 ()
	     (let ((test
		    `(:voice-count 2 
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 36
						:velocity 0))
					     :outputs ((:CV-1 3.0)
						       (:GATE-1 5.0)
						       (:CV-2 0)
						       (:GATE-2 0.0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 48
						:velocity 0))
					     :outputs ((:CV-1 3.0)
						       (:GATE-1 5.0)
						       (:CV-2 4.0)
						       (:GATE-2 5.0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 60
						:velocity 0))
					     :outputs ((:CV-1 5.0)
						       (:GATE-1 0.0)
						       (:CV-2 4.0)
						       (:GATE-2 5.0)))
				    (:events nil
					     :outputs ((:CV-1 5.0)
						       (:GATE-1 5.0)
						       (:CV-2 4.0)
						       (:GATE-2 5.0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

(define-test test-polyphonic-midi-interface-gate-retrigger-2 ()
	     (let ((test
		    `(:voice-count 2 
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 36
						:velocity 0))
					     :outputs ((:CV-1 3.0)
						       (:GATE-1 5.0)
						       (:CV-2 0)
						       (:GATE-2 0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 48
						:velocity 0))
					     :outputs ((:CV-1 3.0)
						       (:GATE-1 5.0)
						       (:CV-2 4.0)
						       (:GATE-2 5.0)))
				    ;; gate must go down for one tick
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 60
						:velocity 0))
					     :outputs ((:CV-1 5.0)
						       (:GATE-1 0.0)
						       (:CV-2 4.0)
						       (:GATE-2 5.0)))
				    (:events nil
					     :outputs ((:CV-1 5.0)
						       (:GATE-1 5.0)
						       (:CV-2 4.0)
						       (:GATE-2 5.0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-off-event
						:channel 1
						:note-number 48
						:velocity 0))
					     :outputs ((:CV-1 5.0)
						       (:GATE-1 5.0)
						       (:CV-2 4.0) ;; CV keeps frequency but Gate goes down
						       (:GATE-2 0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

(define-test test-polyphonic-midi-interface-update-1 ()
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

	       (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-IFC" :midi-events)
	       (let ((midi-ifc (cl-synthesizer:get-module rack "MIDI-IFC")))
		 ;; initial gate must not be nil
		 (assert-true (get-module-output midi-ifc :gate-1))
		 (let ((count (cl-synthesizer-test::output-change-counter
			       :sample-rate 44100
			       :duration-seconds 4
			       :update-fn (lambda () (update-module rack nil))
			       :get-output-fn (lambda ()
						(get-module-output midi-ifc :gate-1)))))
		   (assert-true (< 0.0 count))))))

(define-test test-polyphonic-midi-interface-update-2 ()
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
				      :velocity 100)))))

	       (cl-synthesizer:add-module
		rack "MIDI-IFC"
		#'cl-synthesizer-modules-midi-polyphonic-interface:make-module :voice-count 1)

	       (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-IFC" :midi-events)
	       (let ((midi-ifc (cl-synthesizer:get-module rack "MIDI-IFC")))
		 ;; initial gate must not be nil
		 (assert-true (get-module-output midi-ifc :gate-1))
		 (let ((count (cl-synthesizer-test::output-change-counter
			       :sample-rate 44100
			       :duration-seconds 4
			       :update-fn (lambda () (update-module rack nil))
			       :get-output-fn (lambda ()
						(get-module-output midi-ifc :gate-1)))))
		   (assert-true (< 0.0 count))))))

(define-test test-polyphonic-midi-interface-update-3 ()
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
				      :velocity 100)))))
	       (let ((midi-sequencer (cl-synthesizer:get-module rack "MIDI-SEQUENCER"))
		     (event-count 0))
		 (dotimes (i 44100)
		   (update-module rack nil)
		   (if (get-module-output midi-sequencer :midi-events)
		       (setf event-count (+ 1 event-count))))
		 (assert-equal 2 event-count))))

(define-test test-polyphonic-midi-interface-velocity-1 ()
	     (let ((test
		    `(:voice-count 2 
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 64
						:velocity 0))
					     :outputs ((:VELOCITY-1 0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event
						:channel 1
						:note-number 65
						:velocity 127))
					     :outputs ((:VELOCITY-1 0) (:VELOCITY-2 1270)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

