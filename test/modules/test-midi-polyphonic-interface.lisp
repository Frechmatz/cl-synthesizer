(in-package :cl-synthesizer-test)


;;
;; Voice
;;

(define-test test-polyphonic-voice-manager-voice-2 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice
				     :tick-counter (cl-synthesizer-modules-midi-interface::make-tick-counter)
				     )))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-modules-midi-interface::voice-push-note v 1)
		 (assert-equal 1 current-voice-note)
		 (assert-true (cl-synthesizer-modules-midi-interface::voice-is-note v 1))
		 (assert-nil (cl-synthesizer-modules-midi-interface::voice-is-note v 99)))))

(define-test test-polyphonic-voice-manager-voice-3 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice
				     :tick-counter (cl-synthesizer-modules-midi-interface::make-tick-counter))))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-modules-midi-interface::voice-push-note v 1)
		 (assert-equal 1 current-voice-note))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-modules-midi-interface::voice-push-note v 2)
		 (assert-equal 2 current-voice-note))))

(define-test test-polyphonic-voice-manager-voice-4 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice
				     :tick-counter (cl-synthesizer-modules-midi-interface::make-tick-counter))))
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::voice-push-note v 1))
	       (assert-true (< 1 (cl-synthesizer-modules-midi-interface::voice-push-note v 2)))
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::voice-remove-note v 2))))

(define-test test-polyphonic-voice-manager-voice-5 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice
				     :tick-counter (cl-synthesizer-modules-midi-interface::make-tick-counter))))
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::voice-push-note v 1))
	       (assert-true (< 1 (cl-synthesizer-modules-midi-interface::voice-push-note v 2)))
	       (assert-equal 2 (cl-synthesizer-modules-midi-interface::voice-remove-note v 1))
	       (assert-nil (cl-synthesizer-modules-midi-interface::voice-remove-note v 2))
	       (assert-nil (cl-synthesizer-modules-midi-interface::voice-remove-note v 2))))

(define-test test-polyphonic-voice-manager-voice-6 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice
				     :tick-counter (cl-synthesizer-modules-midi-interface::make-tick-counter))))
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::voice-push-note v 1))
	       (assert-true (< 1 (cl-synthesizer-modules-midi-interface::voice-push-note v 2)))
	       (assert-equal 2 (cl-synthesizer-modules-midi-interface::voice-remove-note v 3))))

(define-test test-polyphonic-voice-manager-voice-7 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice
				     :tick-counter (cl-synthesizer-modules-midi-interface::make-tick-counter))))
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::voice-push-note v 1))
	       (assert-true (< 1 (cl-synthesizer-modules-midi-interface::voice-push-note v 2)))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-modules-midi-interface::voice-push-note v 2)
		 (assert-equal 2 current-voice-note))
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::voice-remove-note v 2))
	       (assert-nil (cl-synthesizer-modules-midi-interface::voice-remove-note v 1))))

(define-test test-polyphonic-voice-manager-voice-8 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice
				     :tick-counter (cl-synthesizer-modules-midi-interface::make-tick-counter))))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-modules-midi-interface::voice-push-note v 1)
		 (assert-equal 1 current-voice-note))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-modules-midi-interface::voice-push-note v 2)
		 (assert-equal 2 current-voice-note))
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::voice-remove-note v 2))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-modules-midi-interface::voice-push-note v 3)
		 (assert-equal 3 current-voice-note))))

(define-test test-polyphonic-voice-manager-voice-9 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice
				     :tick-counter (cl-synthesizer-modules-midi-interface::make-tick-counter))))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-modules-midi-interface::voice-push-note v 1)
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-modules-midi-interface::voice-push-note v 2)
		 (assert-equal 2 current-voice-note))
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::voice-remove-note v 2))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-modules-midi-interface::voice-push-note v 1)
		 (assert-equal 1 current-voice-note)))))

(define-test test-polyphonic-voice-manager-voice-10 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice
				     :tick-counter (cl-synthesizer-modules-midi-interface::make-tick-counter))))
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::voice-push-note v 1))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-modules-midi-interface::voice-push-note v 1)
		 (assert-equal 1 current-voice-note))
	       (assert-nil (cl-synthesizer-modules-midi-interface::voice-remove-note v 1))))




;;
;; Voice-Manager
;;

(defun run-polyphonic-test-case (test-case)
  (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count (getf test-case :voice-count))))
    (dolist (test-case (getf test-case :test-cases))
      (let ((cmd (first test-case))
	    (cmd-arg (second test-case))
	    (expected-voice-number (getf test-case :expected-voice-number))
	    (expected-note (getf test-case :expected-note))
	    )
	(cond
	  ((eq cmd :push)
	   (let ((resulting-voice-number (cl-synthesizer-modules-midi-interface::push-note mgr cmd-arg)))
	     (assert-equal expected-voice-number resulting-voice-number)))
	  ((eq cmd :remove)
	   (multiple-value-bind (resulting-voice-number resulting-note)
	       (cl-synthesizer-modules-midi-interface::remove-note mgr cmd-arg)
	     (assert-equal expected-voice-number resulting-voice-number)
	     (assert-equal expected-note resulting-note)))
	  (t
	   (error "Invalid test case")))))))

;;
;;
;; Tests, where no voice overloading takes place
;;

;; test that A and B are allocated to voices 0 and 1
(define-test test-polyphonic-voice-manager-mgr-no-overload-0 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0)
		       (:push "B" :expected-voice-number 1)))))
	     (run-polyphonic-test-case test)))

;; test that A is removed from voice 0
(define-test test-polyphonic-voice-manager-mgr-no-overload-1 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0)
		       (:push "B" :expected-voice-number 1)
		       (:remove "A" :expected-voice-number 0 :expected-note nil)
		       ))))
	     (run-polyphonic-test-case test)))

;; Test that C is allocated to previously released slot of A
(define-test test-polyphonic-voice-manager-mgr-no-overload-2 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0)
		       (:push "B" :expected-voice-number 1)
		       (:remove "A" :expected-voice-number 0 :expected-note nil)
		       (:push "C" :expected-voice-number 0)
		       ))))
	     (run-polyphonic-test-case test)))

;; Test that C is allocated to previously released slot of B
(define-test test-polyphonic-voice-manager-mgr-no-overload-3 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0)
		       (:push "B" :expected-voice-number 1)
		       (:remove "B" :expected-voice-number 1 :expected-note nil)
		       (:push "C" :expected-voice-number 1)
		       ))))
	     (run-polyphonic-test-case test)))

;; Test that A (voice 0) is playing after multiple adds/removes to voice 1
(define-test test-polyphonic-voice-manager-mgr-no-overload-4 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0)
		       (:push "B" :expected-voice-number 1)
		       (:remove "B" :expected-voice-number 1 :expected-note nil)
		       (:push "C" :expected-voice-number 1)
		       (:remove "C" :expected-voice-number 1 :expected-note nil)
		       (:push "D" :expected-voice-number 1)
		       ))))
	     (run-polyphonic-test-case test)))

(define-test test-polyphonic-voice-manager-mgr-no-overload-5 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0)
		       (:push "B" :expected-voice-number 1)
		       (:remove "A" :expected-voice-number 0 :expected-note nil)
		       (:push "C" :expected-voice-number 0)

		       ))))
	     (run-polyphonic-test-case test)))

;; both voices played and released, next note will be assigned to first released voice
(define-test test-polyphonic-voice-manager-mgr-no-overload-6 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0)
		       (:push "B" :expected-voice-number 1)
		       (:remove "A" :expected-voice-number 0 :expected-note nil)
		       (:remove "B" :expected-voice-number 1 :expected-note nil)
		       (:push "C" :expected-voice-number 0)
		       ))))
	     (run-polyphonic-test-case test)))

;; both voices played and released, next note will be assigned to first released voice
(define-test test-polyphonic-voice-manager-mgr-no-overload-7 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0)
		       (:push "B" :expected-voice-number 1)
		       (:remove "B" :expected-voice-number 1 :expected-note nil)
		       (:remove "A" :expected-voice-number 0 :expected-note nil)
		       (:push "C" :expected-voice-number 1)
		       ))))
	     (run-polyphonic-test-case test)))

;;
;;
;; Tests, where voice overloading takes place
;; Test cases where all voices are occupied and new notes are to be pushed
;;
;;

(define-test test-polyphonic-voice-manager-mgr-overload-0 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0)
		       (:push "B" :expected-voice-number 1)
		       (:push "C" :expected-voice-number 0)
		       (:push "D" :expected-voice-number 1)
		       (:push "E" :expected-voice-number 0)
		       (:push "F" :expected-voice-number 1)
		       ))))
	     (run-polyphonic-test-case test)))

;; Fast ON/OFF of notes when all voices are playing. Current note should be consecutively assigned to same voice index
(define-test test-polyphonic-voice-manager-mgr-overload-1 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0)
		       (:push "B" :expected-voice-number 1)
		       (:push "C-ON" :expected-voice-number 0)
		       (:remove "C-ON" :expected-voice-number 0 :expected-note nil)
		       (:push "D-ON" :expected-voice-number 0)
		       ))))
	     (run-polyphonic-test-case test)))

;; Fast ON/OFF of notes when all voices are playing. Current note should be consecutively assigned to same voice index
(define-test test-polyphonic-voice-manager-mgr-overload-2 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0)
		       (:push "B" :expected-voice-number 1)
		       (:push "C" :expected-voice-number 0)
		       (:push "D-ON" :expected-voice-number 1)
		       (:remove "D-ON" :expected-voice-number 1 :expected-note nil)
		       (:push "D-ON" :expected-voice-number 1)
		       ))))
	     (run-polyphonic-test-case test)))

(define-test test-polyphonic-voice-manager-mgr-overload-3 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0)
		       (:push "B" :expected-voice-number 1)
		       (:push "C" :expected-voice-number 0)
		       (:remove "B" :expected-voice-number 1 :expected-note nil)
		       ))))
	     (run-polyphonic-test-case test)))



;;
;; Midi-Interface
;;

(defun test-polyphonic-midi-interface-make-midi-interface
    (voice-count
     &key
       (play-mode :PLAY-MODE-POLY)
       (force-gate-retrigger nil)
       (channel nil))
  (cl-synthesizer-modules-midi-interface:make-module
   "Test-Midi-Interface"
   (cl-synthesizer:make-environment)
   :voice-count voice-count
   :channel channel
   :force-gate-retrigger force-gate-retrigger
   :note-number-to-cv (lambda (n) (* 1000 n))
   :play-mode play-mode))

(defun run-polyphonic-test-case-midi-ifc (test-case)
  (let ((ifc (test-polyphonic-midi-interface-make-midi-interface
	      (getf test-case :voice-count)
	      :play-mode (getf test-case :play-mode)
	      :force-gate-retrigger (getf test-case :force-gate-retrigger)
	      :channel (getf test-case :channel)
	      )))
    (dolist (cur-test-case (getf test-case :test-cases))
      (update-module ifc (list :midi-events (getf cur-test-case :events)))
      (dolist (cur-output (getf cur-test-case :outputs))
	(assert-equal (float (second cur-output)) (float (get-module-output ifc (first cur-output))))))))

(define-test test-polyphonic-midi-interface-1 ()
	     (let ((test
		    '(:voice-count 2 :play-mode :PLAY-MODE-POLY
		      :test-cases
		      ((:events nil :outputs ((:CV-1 0) (:CV-2 0) (:GATE-1 0) (:GATE-2 0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

(define-test test-polyphonic-midi-interface-2 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-POLY
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
					     :outputs ((:CV-1 64000)
						       (:GATE-1 5.0)
						       (:CV-2 0)
						       (:GATE-2 0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))


(define-test test-polyphonic-midi-interface-3 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-POLY
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
					     :outputs ((:CV-1 64000)
						       (:GATE-1 5.0)
						       (:CV-2 0)
						       (:GATE-2 0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
					     :outputs ((:CV-1 64000)
						       (:GATE-1 5.0)
						       (:CV-2 32000)
						       (:GATE-2 5.0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

;;
;; Test default note to cv conversion
;;

(define-test test-polyphonic-midi-interface-default-note-to-cv ()
	     (let ((ifc (cl-synthesizer-modules-midi-interface:make-module
			 "MIDI-IFC"
			 (cl-synthesizer:make-environment))))
	       (update-module ifc (list :midi-events (list (cl-synthesizer-midi-event:make-note-on-event 1 24 0))))
	       (assert-equal 2.0 (get-module-output ifc :cv-1))))

;;
;; unisono tests
;;
(define-test test-polyphonic-midi-interface-unisono-1 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-UNISONO
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
					     :outputs ((:CV-1 64000)
						       (:GATE-1 5.0)
						       (:CV-2 64000)
						       (:GATE-2 5.0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
					     :outputs ((:CV-1 32000)
						       (:GATE-1 5.0)
						       (:CV-2 32000)
						       (:GATE-2 5.0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

(define-test test-polyphonic-midi-interface-unisono-2 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-UNISONO
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
					     :outputs ((:CV-1 64000)
						       (:GATE-1 5.0)
						       (:CV-2 64000)
						       (:GATE-2 5.0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
					     :outputs ((:CV-1 32000)
						       (:GATE-1 5.0)
						       (:CV-2 32000)
						       (:GATE-2 5.0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-off-event 1 32 0))
					     :outputs ((:CV-1 64000)
						       (:GATE-1 5.0)
						       (:CV-2 64000)
						       (:GATE-2 5.0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

(define-test test-polyphonic-midi-interface-unisono-3 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-UNISONO
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
					     :outputs ((:CV-1 64000)
						       (:GATE-1 5.0)
						       (:CV-2 64000)
						       (:GATE-2 5.0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
					     :outputs ((:CV-1 32000)
						       (:GATE-1 5.0)
						       (:CV-2 32000)
						       (:GATE-2 5.0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-off-event 1 64 0))
					     :outputs ((:CV-1 32000)
						       (:GATE-1 5.0)
						       (:CV-2 32000)
						       (:GATE-2 5.0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-off-event 1 32 0))
					     :outputs ((:CV-1 32000)
						       (:GATE-1 0)
						       (:CV-2 32000)
						       (:GATE-2 0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

;;
;; tests for update with multiple midi-events
;;
(define-test test-polyphonic-midi-interface-multiple-events-1 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-POLY
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0)
					       ,(cl-synthesizer-midi-event:make-note-off-event 1 64 0))
					     :outputs ((:CV-1 64000)
						       (:GATE-1 0)
						       (:CV-2 0)
						       (:GATE-2 0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

(define-test test-polyphonic-midi-interface-multiple-events-2 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-POLY
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0)
					       ,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
					     :outputs ((:CV-1 64000)
						       (:GATE-1 5.0)
						       (:CV-2 32000)
						       (:GATE-2 5.0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

(define-test test-polyphonic-midi-interface-multiple-events-3 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-POLY
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0)
					       ,(cl-synthesizer-midi-event:make-note-on-event 1 32 0)
					       ,(cl-synthesizer-midi-event:make-note-off-event 1 64 0))
					     :outputs ((:CV-1 64000)
						       (:GATE-1 0)
						       (:CV-2 32000)
						       (:GATE-2 5.0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

;; Test channel support

(define-test test-polyphonic-midi-interface-channel-1 ()
	     (let ((test
		    `(:voice-count 1 :play-mode :PLAY-MODE-POLY :channel 2
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
					     :outputs ((:CV-1 0)
						       (:GATE-1 0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

(define-test test-polyphonic-midi-interface-channel-2 ()
	     (let ((test
		    `(:voice-count 1 :play-mode :PLAY-MODE-POLY :channel 1
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
					     :outputs ((:CV-1 64000)
						       (:GATE-1 5.0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))



;;
;; Tests that check that gate goes down for one tick when a voice
;; is stolen in polyphonic mode
;;

(define-test test-polyphonic-midi-interface-gate-retrigger-1 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-POLY
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
					     :outputs ((:CV-1 32000)
						       (:GATE-1 5.0)
						       (:CV-2 0)
						       (:GATE-2 0.0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 48 0))
					     :outputs ((:CV-1 32000)
						       (:GATE-1 5.0)
						       (:CV-2 48000)
						       (:GATE-2 5.0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
					     :outputs ((:CV-1 64000)
						       (:GATE-1 0.0)
						       (:CV-2 48000)
						       (:GATE-2 5.0)))
				    (:events nil
					     :outputs ((:CV-1 64000)
						       (:GATE-1 5.0)
						       (:CV-2 48000)
						       (:GATE-2 5.0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

(define-test test-polyphonic-midi-interface-gate-retrigger-2 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-POLY
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
					     :outputs ((:CV-1 32000)
						       (:GATE-1 5.0)
						       (:CV-2 0)
						       (:GATE-2 0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 48 0))
					     :outputs ((:CV-1 32000)
						       (:GATE-1 5.0)
						       (:CV-2 48000)
						       (:GATE-2 5.0)))
				    ;; gate must go down for one tick
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
					     :outputs ((:CV-1 64000)
						       (:GATE-1 0.0)
						       (:CV-2 48000)
						       (:GATE-2 5.0)))
				    (:events nil
					     :outputs ((:CV-1 64000)
						       (:GATE-1 5.0)
						       (:CV-2 48000)
						       (:GATE-2 5.0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-off-event 1 48 0))
					     :outputs ((:CV-1 64000)
						       (:GATE-1 5.0)
						       (:CV-2 48000) ;; CV keeps frequency but Gate goes down
						       (:GATE-2 0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

;;
;; Tests that check that gate goes not down for one tick when a voice
;; is overloaded in unisono mode
;;

(define-test test-polyphonic-midi-interface-gate-retrigger-3 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-UNISONO
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
					     :outputs ((:CV-1 32000)
						       (:GATE-1 5.0)
						       (:CV-2 32000)
						       (:GATE-2 5.0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 48 0))
					     :outputs ((:CV-1 48000)
						       (:GATE-1 5.0)
						       (:CV-2 48000)
						       (:GATE-2 5.0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
					     :outputs ((:CV-1 64000)
						       (:GATE-1 5.0)
						       (:CV-2 64000)
						       (:GATE-2 5.0)))))))
	       (run-polyphonic-test-case-midi-ifc test)))

;;
;; Tests that check that gate goes down for one tick
;; in unisono mode and when re-triggering of gate has been activated
;;

(define-test test-polyphonic-midi-interface-gate-retrigger-4 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-UNISONO :force-gate-retrigger t
				   :test-cases
				   ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
					     :outputs ((:CV-1 32000)
						       (:GATE-1 5.0)
						       (:CV-2 32000)
						       (:GATE-2 5.0)))
				    (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 48 0))
					     :outputs ((:CV-1 48000)
						       (:GATE-1 0.0)
						       (:CV-2 48000)
						       (:GATE-2 0.0)))
				    (:events nil
					     :outputs ((:CV-1 48000)
						       (:GATE-1 5.0)
						       (:CV-2 48000)
						       (:GATE-2 5.0)))))))
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
		#'cl-synthesizer-modules-midi-interface:make-module :voice-count 1)

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
				     (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
		 (list :timestamp-milli-seconds 700
		       :midi-events (list
				     (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))))

	       (cl-synthesizer:add-module
		rack "MIDI-IFC"
		#'cl-synthesizer-modules-midi-interface:make-module :voice-count 1)

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
				     (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
		 (list :timestamp-milli-seconds 700
		       :midi-events (list
				     (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))))
	       (let ((midi-sequencer (cl-synthesizer:get-module rack "MIDI-SEQUENCER"))
		     (event-count 0))
		 (dotimes (i 44100)
		   (update-module rack nil)
		   (if (get-module-output midi-sequencer :midi-events)
		       (setf event-count (+ 1 event-count))))
		 (assert-equal 2 event-count))))

		 
