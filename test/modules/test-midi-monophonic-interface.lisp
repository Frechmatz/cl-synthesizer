(in-package :cl-synthesizer-test)

(defun test-monophonic-midi-interface-make-midi-interface
    (&key
       (force-gate-retrigger nil)
       (channel nil)
       (force-velocity-update nil)
       (cv-velocity-max 1270))
  (cl-synthesizer-modules-midi-monophonic-interface:make-module
   "Test-Midi-Interface"
   (cl-synthesizer:make-environment)
   :channel channel
   :force-gate-retrigger force-gate-retrigger
   :force-velocity-update force-velocity-update
   :cv-velocity-max cv-velocity-max))

(defun run-monophonic-test-case-midi-ifc (test-case)
  (let ((ifc (test-monophonic-midi-interface-make-midi-interface
	      :force-gate-retrigger (getf test-case :force-gate-retrigger)
	      :channel (getf test-case :channel)
	      :force-velocity-update (getf test-case :force-velocity-update))))
    (dolist (cur-test-case (getf test-case :test-cases))
      (update-module ifc (list :midi-events (getf cur-test-case :events)))
      (dolist (cur-output (getf cur-test-case :outputs))
	(assert-equal (float (second cur-output)) (float (get-module-output ifc (first cur-output))))))))

(define-test test-monophonic-midi-interface-1 ()
	     (let ((test
		    `(:test-cases
		      ((:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 60
				   :velocity 0))
				:outputs ((:CV 5.0)
					  (:GATE 5.0)
					  (:VELOCITY 0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 36
				   :velocity 0))
				:outputs ((:CV 3.0)
					  (:GATE 5.0)))))))
	       (run-monophonic-test-case-midi-ifc test)))

(define-test test-monophonic-midi-interface-2 ()
	     (let ((test
		    `(:test-cases
		      ((:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 60
				   :velocity 0))
				:outputs ((:CV 5.0)
					  (:GATE 5.0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 36
				   :velocity 0))
				:outputs ((:CV 3.0)
					  (:GATE 5.0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-off-event
				   :channel 1
				   :note-number 36
				   :velocity 0))
				:outputs ((:CV 5.0)
					  (:GATE 5.0)))))))
	       (run-monophonic-test-case-midi-ifc test)))

(define-test test-monophonic-midi-interface-3 ()
	     (let ((test
		    `(:test-cases
		      ((:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 60
				   :velocity 0))
				:outputs ((:CV 5.0)
					  (:GATE 5.0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 36
				   :velocity 0))
				:outputs ((:CV 3.0)
					  (:GATE 5.0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-off-event
				   :channel 1
				   :note-number 60
				   :velocity 0))
				:outputs ((:CV 3.0)
					  (:GATE 5.0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-off-event
				   :channel 1
				   :note-number 36
				   :velocity 0))
				:outputs ((:CV 3.0)
					  (:GATE 0)))))))
	       (run-monophonic-test-case-midi-ifc test)))

;;
;; Tests that checks that the gate goes not down for one tick when a voice
;; is overloaded
;;

(define-test test-monophonic-midi-interface-gate-retrigger-3 ()
	     (let ((test
		    `(:test-cases
		      ((:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 36
				   :velocity 0))
				:outputs ((:CV 3.0)
					  (:GATE 5.0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 48
				   :velocity 0))
				:outputs ((:CV 4.0)
					  (:GATE 5.0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 60
				   :velocity 0))
				:outputs ((:CV 5.0)
					  (:GATE 5.0)))))))
	       (run-monophonic-test-case-midi-ifc test)))

;;
;; Tests that check that gate goes down for one tick
;; when re-triggering of gate has been activated
;;

(define-test test-monophonic-midi-interface-gate-retrigger-4 ()
	     (let ((test
		    `(:force-gate-retrigger t
					    :test-cases
					    ((:events (,(cl-synthesizer-midi-event:make-note-on-event
							 :channel 1
							 :note-number 36
							 :velocity 0))
						      :outputs ((:CV 3.0)
								(:GATE 5.0)))
					     (:events (,(cl-synthesizer-midi-event:make-note-on-event
							 :channel 1
							 :note-number 48
							 :velocity 0))
						      :outputs ((:CV 4.0)
								(:GATE 0.0)))
					     (:events nil
						      :outputs ((:CV 4.0)
								(:GATE 5.0)))))))
	       (run-monophonic-test-case-midi-ifc test)))


(define-test test-monophonic-midi-interface-update-1 ()
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
		#'cl-synthesizer-modules-midi-monophonic-interface:make-module)

	       (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-IFC" :midi-events)
	       (let ((midi-ifc (cl-synthesizer:get-module rack "MIDI-IFC")))
		 ;; initial gate must not be nil
		 (assert-true (get-module-output midi-ifc :gate))
		 (let ((count (cl-synthesizer-test::output-change-counter
			       :sample-rate 44100
			       :duration-seconds 4
			       :update-fn (lambda () (update-module rack nil))
			       :get-output-fn (lambda ()
						(get-module-output midi-ifc :gate)))))
		   (assert-true (< 0.0 count))))))

(define-test test-monophonic-midi-interface-update-2 ()
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
		#'cl-synthesizer-modules-midi-monophonic-interface:make-module)

	       (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-IFC" :midi-events)
	       (let ((midi-ifc (cl-synthesizer:get-module rack "MIDI-IFC")))
		 ;; initial gate must not be nil
		 (assert-true (get-module-output midi-ifc :gate))
		 (let ((count (cl-synthesizer-test::output-change-counter
			       :sample-rate 44100
			       :duration-seconds 4
			       :update-fn (lambda () (update-module rack nil))
			       :get-output-fn (lambda ()
						(get-module-output midi-ifc :gate)))))
		   (assert-true (< 0.0 count))))))

;;
;; Velocity tests
;;

;; Sample velocity on first note on event
(define-test test-monophonic-midi-interface-velocity-1 ()
	     (let ((test
		    `(:test-cases
		      ((:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 60
				   :velocity 127))
				:outputs ((:CV 5.0)
					  (:GATE 5.0)
					  (:VELOCITY 1270)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 36
				   :velocity 0))
				:outputs ((:CV 3.0)
					  (:GATE 5.0)
					  (:VELOCITY 1270)))
		       (:events (,(cl-synthesizer-midi-event:make-note-off-event
				   :channel 1
				   :note-number 60
				   :velocity 0))
				:outputs ((:CV 3.0)
					  (:GATE 5.0)
					  (:VELOCITY 1270)))
		       (:events (,(cl-synthesizer-midi-event:make-note-off-event
				   :channel 1
				   :note-number 36
				   :velocity 0))
				:outputs ((:CV 3.0)
					  (:GATE 0)
					  (:VELOCITY 1270)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 36
				   :velocity 1))
				:outputs ((:CV 3.0)
					  (:GATE 5.0)
					  (:VELOCITY 10)))))))
	       (run-monophonic-test-case-midi-ifc test)))

;; Sample velocity on all note on event
(define-test test-monophonic-midi-interface-velocity-2 ()
	     (let ((test
		    `(:force-velocity-update
		      t
		      :test-cases
		      ((:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 60
				   :velocity 127))
				:outputs ((:CV 5.0)
					  (:GATE 5.0)
					  (:VELOCITY 1270)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 36
				   :velocity 1))
				:outputs ((:CV 3.0)
					  (:GATE 5.0)
					  (:VELOCITY 10)))
		       (:events (,(cl-synthesizer-midi-event:make-note-off-event
				   :channel 1
				   :note-number 60
				   :velocity 0))
				:outputs ((:CV 3.0)
					  (:GATE 5.0)
					  (:VELOCITY 10)))
		       (:events (,(cl-synthesizer-midi-event:make-note-off-event
				   :channel 1
				   :note-number 36
				   :velocity 0))
				:outputs ((:CV 3.0)
					  (:GATE 0)
					  (:VELOCITY 10)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 36
				   :velocity 127))
				:outputs ((:CV 3.0)
					  (:GATE 5.0)
					  (:VELOCITY 1270)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event
				   :channel 1
				   :note-number 36
				   :velocity 1))
				:outputs ((:CV 3.0)
					  (:GATE 5.0)
					  (:VELOCITY 10)))))))
	       (run-monophonic-test-case-midi-ifc test)))

