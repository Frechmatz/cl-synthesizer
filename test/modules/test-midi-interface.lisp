(in-package :cl-synthesizer-test)

(defun test-midi-interface-make-midi-interface (voice-count &key (play-mode :PLAY-MODE-POLY))
  (cl-synthesizer-modules-midi-interface:midi-interface
   "Test-Midi-Interface"
   (cl-synthesizer::make-environment)
   :voice-count voice-count
   :note-number-to-cv (lambda (n) (* 1000 n))
   :play-mode play-mode))

(defun test-midi-interface-get-output (ifc output)
  (funcall (getf ifc :get-output) output))

(defun test-midi-interface-update (ifc events)
  (funcall (getf ifc :update) :midi-events events))

(defun run-test-case-midi-ifc (test-case)
  (let ((ifc (test-midi-interface-make-midi-interface (getf test-case :voice-count) :play-mode (getf test-case :play-mode))))
    (dolist (cur-test-case (getf test-case :test-cases))
      (test-midi-interface-update ifc (getf cur-test-case :events))
      (dolist (cur-output (getf cur-test-case :outputs))
	(assert-equal (second cur-output) (test-midi-interface-get-output ifc (first cur-output)))))))

(define-test test-midi-interface-1 ()
	     (let ((test
		    '(:voice-count 2 :play-mode :PLAY-MODE-POLY
		      :test-cases
		      ((:events nil :outputs ((:CV-1 0) (:CV-2 0) (:GATE-1 0) (:GATE-2 0)))))))
	       (run-test-case-midi-ifc test)))

(define-test test-midi-interface-2 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-POLY
		      :test-cases
		      ((:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
			       :outputs ((:CV-1 64000)
					 (:GATE-1 5.0)
					 (:CV-2 0)
					 (:GATE-2 0)))))))
	       (run-test-case-midi-ifc test)))


(define-test test-midi-interface-3 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-POLY
		      :test-cases
		      (
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
			       :outputs ((:CV-1 64000)
					 (:GATE-1 5.0)
					 (:CV-2 0)
					 (:GATE-2 0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
			       :outputs ((:CV-1 64000)
					 (:GATE-1 5.0)
					 (:CV-2 32000)
					 (:GATE-2 5.0)))


		       ))))
	       (run-test-case-midi-ifc test)))

(define-test test-midi-interface-4 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-POLY
		      :test-cases
		      (
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
			       :outputs ((:CV-1 32000)
					 (:GATE-1 5.0)
					 (:CV-2 0)
					 (:GATE-2 0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 48 0))
			       :outputs ((:CV-1 32000)
					 (:GATE-1 5.0)
					 (:CV-2 48000)
					 (:GATE-2 5.0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
			       :outputs ((:CV-1 64000)
					 (:GATE-1 5.0)
					 (:CV-2 48000)
					 (:GATE-2 5.0)))
		       ))))
	       (run-test-case-midi-ifc test)))

;; die mittlere Note wegnehmen
(define-test test-midi-interface-4 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-POLY
		      :test-cases
		      (
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
			       :outputs ((:CV-1 32000)
					 (:GATE-1 5.0)
					 (:CV-2 0)
					 (:GATE-2 0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 48 0))
			       :outputs ((:CV-1 32000)
					 (:GATE-1 5.0)
					 (:CV-2 48000)
					 (:GATE-2 5.0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
			       :outputs ((:CV-1 64000)
					 (:GATE-1 5.0)
					 (:CV-2 48000)
					 (:GATE-2 5.0)))
		       
		       (:events (,(cl-synthesizer-midi-event:make-note-off-event 1 48 0))
			       :outputs ((:CV-1 64000)
					 (:GATE-1 5.0)
					 (:CV-2 48000) ;; CV keeps frequency but Gate goes down
					 (:GATE-2 0)))
		       		       
		       ))))
	       (run-test-case-midi-ifc test)))

;;
;; unisono tests
;;
(define-test test-midi-interface-unisono-1 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-UNISONO
		      :test-cases
		      (
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
			       :outputs ((:CV-1 64000)
					 (:GATE-1 5.0)
					 (:CV-2 64000)
					 (:GATE-2 5.0)))
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
			       :outputs ((:CV-1 32000)
					 (:GATE-1 5.0)
					 (:CV-2 32000)
					 (:GATE-2 5.0)))
		       ))))
	       (run-test-case-midi-ifc test)))

(define-test test-midi-interface-unisono-2 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-UNISONO
		      :test-cases
		      (
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
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
					 (:GATE-2 5.0)))
		       
		       ))))
	       (run-test-case-midi-ifc test)))

(define-test test-midi-interface-unisono-3 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-UNISONO
		      :test-cases
		      (
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0))
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
					 (:GATE-2 0)))
		       
		       ))))
	       (run-test-case-midi-ifc test)))

;;
;; tests for update with multiple midi-events
;;
(define-test test-midi-interface-multiple-events-1 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-POLY
		      :test-cases
		      (
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0)
				  ,(cl-synthesizer-midi-event:make-note-off-event 1 64 0))
			       :outputs ((:CV-1 64000)
					 (:GATE-1 0)
					 (:CV-2 0)
					 (:GATE-2 0)))
		       ))))
	       (run-test-case-midi-ifc test)))

(define-test test-midi-interface-multiple-events-2 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-POLY
		      :test-cases
		      (
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0)
				  ,(cl-synthesizer-midi-event:make-note-on-event 1 32 0))
			       :outputs ((:CV-1 64000)
					 (:GATE-1 5.0)
					 (:CV-2 32000)
					 (:GATE-2 5.0)))
		       ))))
	       (run-test-case-midi-ifc test)))

(define-test test-midi-interface-multiple-events-3 ()
	     (let ((test
		    `(:voice-count 2 :play-mode :PLAY-MODE-POLY
		      :test-cases
		      (
		       (:events (,(cl-synthesizer-midi-event:make-note-on-event 1 64 0)
				  ,(cl-synthesizer-midi-event:make-note-on-event 1 32 0)
				  ,(cl-synthesizer-midi-event:make-note-off-event 1 64 0))
			       :outputs ((:CV-1 64000)
					 (:GATE-1 0)
					 (:CV-2 32000)
					 (:GATE-2 5.0)))
		       ))))
	       (run-test-case-midi-ifc test)))
