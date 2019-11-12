(in-package :cl-synthesizer-test)

(defun test-monophonic-midi-interface-make-midi-interface
    (&key
       (force-gate-retrigger nil)
       (channel nil))
  (cl-synthesizer-modules-midi-monophonic-interface:make-module
   "Test-Midi-Interface"
   (cl-synthesizer:make-environment)
   :channel channel
   :force-gate-retrigger force-gate-retrigger
   :note-number-to-cv (lambda (n) (* 1000 n))))

(defun run-monophonic-test-case-midi-ifc (test-case)
  (let ((ifc (test-monophonic-midi-interface-make-midi-interface
	      :force-gate-retrigger (getf test-case :force-gate-retrigger)
	      :channel (getf test-case :channel)
	      )))
    (dolist (cur-test-case (getf test-case :test-cases))
      (update-module ifc (list :midi-events (getf cur-test-case :events)))
      (dolist (cur-output (getf cur-test-case :outputs))
	(assert-equal (float (second cur-output)) (float (get-module-output ifc (first cur-output))))))))

;;
;; Test default note to cv conversion
;;

(define-test test-monophonic-midi-interface-default-note-to-cv ()
	     (let ((ifc (cl-synthesizer-modules-midi-monophonic-interface:make-module
			 "MIDI-IFC"
			 (cl-synthesizer:make-environment))))
	       (update-module ifc (list :midi-events (list (cl-synthesizer-midi-event:make-note-on-event 1 24 0))))
	       (assert-equal 2.0 (get-module-output ifc :cv-1))))

;;
;; unisono tests
;;
(define-test test-monophonic-midi-interface-unisono-1 ()
	     (let ((test
		    `(:test-cases
		      ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
				:outputs ((:CV-1 64000)
					  (:GATE-1 5.0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
				:outputs ((:CV-1 32000)
					  (:GATE-1 5.0)))))))
	       (run-monophonic-test-case-midi-ifc test)))

(define-test test-monophonic-midi-interface-unisono-2 ()
	     (let ((test
		    `(:test-cases
		      ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
				:outputs ((:CV-1 64000)
					  (:GATE-1 5.0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
				:outputs ((:CV-1 32000)
					  (:GATE-1 5.0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-off-event 1 32 0))
				:outputs ((:CV-1 64000)
					  (:GATE-1 5.0)))
		       
		       ))))
	       (run-monophonic-test-case-midi-ifc test)))

(define-test test-monophonic-midi-interface-unisono-3 ()
	     (let ((test
		    `(:test-cases
		      ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
				:outputs ((:CV-1 64000)
					  (:GATE-1 5.0)
					  ))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
				:outputs ((:CV-1 32000)
					  (:GATE-1 5.0)
					  ))
		       (:events (,(cl-synthesizer-midi-event:make-note-off-event 1 64 0))
				:outputs ((:CV-1 32000)
					  (:GATE-1 5.0)
					  ))
		       (:events (,(cl-synthesizer-midi-event:make-note-off-event 1 32 0))
				:outputs ((:CV-1 32000)
					  (:GATE-1 0)
					  ))))))
	       (run-monophonic-test-case-midi-ifc test)))



;;
;; Tests that check that gate goes not down for one tick when a voice
;; is overloaded in unisono mode
;;

(define-test test-monophonic-midi-interface-gate-retrigger-3 ()
	     (let ((test
		    `(:test-cases
		      ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
				:outputs ((:CV-1 32000)
					  (:GATE-1 5.0)
					  ))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 48 0))
				:outputs ((:CV-1 48000)
					  (:GATE-1 5.0)
					  ))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
				:outputs ((:CV-1 64000)
					  (:GATE-1 5.0)
					  ))))))
	       (run-monophonic-test-case-midi-ifc test)))

;;
;; Tests that check that gate goes down for one tick
;; in unisono mode and when re-triggering of gate has been activated
;;

(define-test test-monophonic-midi-interface-gate-retrigger-4 ()
	     (let ((test
		    `(:force-gate-retrigger t
					    :test-cases
					    ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
						      :outputs ((:CV-1 32000)
								(:GATE-1 5.0)
								))
					     (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 48 0))
						      :outputs ((:CV-1 48000)
								(:GATE-1 0.0)
								))
					     (:events nil
						      :outputs ((:CV-1 48000)
								(:GATE-1 5.0)
								))))))
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
				     (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
		 (list :timestamp-milli-seconds 700
		       :midi-events (list
				     (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))
		 (list :timestamp-milli-seconds 1800
		       :midi-events (list
				     (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
		 (list :timestamp-milli-seconds 2100
		       :midi-events (list
				     (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))))

	       (cl-synthesizer:add-module
		rack "MIDI-IFC"
		#'cl-synthesizer-modules-midi-monophonic-interface:make-module)

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
				     (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
		 (list :timestamp-milli-seconds 700
		       :midi-events (list
				     (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))))

	       (cl-synthesizer:add-module
		rack "MIDI-IFC"
		#'cl-synthesizer-modules-midi-monophonic-interface:make-module)

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

