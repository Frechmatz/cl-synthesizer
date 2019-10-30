(in-package :cl-synthesizer-test)

;;
;; voice tests
;;

(define-test test-voice-manager-voice-2 ()
	     (let ((v (make-instance 'cl-synthesizer-midi-voice-manager::voice
				     :tick-counter (cl-synthesizer-midi-voice-manager::make-tick-counter)
				     )))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-midi-voice-manager::voice-push-note v 1)
		 (assert-equal 1 current-voice-note)
		 (assert-true (cl-synthesizer-midi-voice-manager::voice-is-note v 1))
		 (assert-nil (cl-synthesizer-midi-voice-manager::voice-is-note v 99)))))

(define-test test-voice-manager-voice-3 ()
	     (let ((v (make-instance 'cl-synthesizer-midi-voice-manager::voice
				     :tick-counter (cl-synthesizer-midi-voice-manager::make-tick-counter))))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-midi-voice-manager::voice-push-note v 1)
		 (assert-equal 1 current-voice-note))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-midi-voice-manager::voice-push-note v 2)
		 (assert-equal 2 current-voice-note))))

(define-test test-voice-manager-voice-4 ()
	     (let ((v (make-instance 'cl-synthesizer-midi-voice-manager::voice
				     :tick-counter (cl-synthesizer-midi-voice-manager::make-tick-counter))))
	       (assert-equal 1 (cl-synthesizer-midi-voice-manager::voice-push-note v 1))
	       (assert-true (< 1 (cl-synthesizer-midi-voice-manager::voice-push-note v 2)))
	       (assert-equal 1 (cl-synthesizer-midi-voice-manager::voice-remove-note v 2))))

(define-test test-voice-manager-voice-5 ()
	     (let ((v (make-instance 'cl-synthesizer-midi-voice-manager::voice
				     :tick-counter (cl-synthesizer-midi-voice-manager::make-tick-counter))))
	       (assert-equal 1 (cl-synthesizer-midi-voice-manager::voice-push-note v 1))
	       (assert-true (< 1 (cl-synthesizer-midi-voice-manager::voice-push-note v 2)))
	       (assert-equal 2 (cl-synthesizer-midi-voice-manager::voice-remove-note v 1))
	       (assert-nil (cl-synthesizer-midi-voice-manager::voice-remove-note v 2))
	       (assert-nil (cl-synthesizer-midi-voice-manager::voice-remove-note v 2))))

(define-test test-voice-manager-voice-6 ()
	     (let ((v (make-instance 'cl-synthesizer-midi-voice-manager::voice
				     :tick-counter (cl-synthesizer-midi-voice-manager::make-tick-counter))))
	       (assert-equal 1 (cl-synthesizer-midi-voice-manager::voice-push-note v 1))
	       (assert-true (< 1 (cl-synthesizer-midi-voice-manager::voice-push-note v 2)))
	       (assert-equal 2 (cl-synthesizer-midi-voice-manager::voice-remove-note v 3))))

(define-test test-voice-manager-voice-7 ()
	     (let ((v (make-instance 'cl-synthesizer-midi-voice-manager::voice
				     :tick-counter (cl-synthesizer-midi-voice-manager::make-tick-counter))))
	       (assert-equal 1 (cl-synthesizer-midi-voice-manager::voice-push-note v 1))
	       (assert-true (< 1 (cl-synthesizer-midi-voice-manager::voice-push-note v 2)))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-midi-voice-manager::voice-push-note v 2)
		 (assert-equal 2 current-voice-note))
	       (assert-equal 1 (cl-synthesizer-midi-voice-manager::voice-remove-note v 2))
	       (assert-nil (cl-synthesizer-midi-voice-manager::voice-remove-note v 1))))

(define-test test-voice-manager-voice-8 ()
	     (let ((v (make-instance 'cl-synthesizer-midi-voice-manager::voice
				     :tick-counter (cl-synthesizer-midi-voice-manager::make-tick-counter))))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-midi-voice-manager::voice-push-note v 1)
		 (assert-equal 1 current-voice-note))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-midi-voice-manager::voice-push-note v 2)
		 (assert-equal 2 current-voice-note))
	       (assert-equal 1 (cl-synthesizer-midi-voice-manager::voice-remove-note v 2))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-midi-voice-manager::voice-push-note v 3)
		 (assert-equal 3 current-voice-note))))

(define-test test-voice-manager-voice-9 ()
	     (let ((v (make-instance 'cl-synthesizer-midi-voice-manager::voice
				     :tick-counter (cl-synthesizer-midi-voice-manager::make-tick-counter))))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-midi-voice-manager::voice-push-note v 1)
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-midi-voice-manager::voice-push-note v 2)
		 (assert-equal 2 current-voice-note))
	       (assert-equal 1 (cl-synthesizer-midi-voice-manager::voice-remove-note v 2))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-midi-voice-manager::voice-push-note v 1)
		 (assert-equal 1 current-voice-note)))))

(define-test test-voice-manager-voice-10 ()
	     (let ((v (make-instance 'cl-synthesizer-midi-voice-manager::voice
				     :tick-counter (cl-synthesizer-midi-voice-manager::make-tick-counter))))
	       (assert-equal 1 (cl-synthesizer-midi-voice-manager::voice-push-note v 1))
	       (multiple-value-bind (current-voice-note)
		   (cl-synthesizer-midi-voice-manager::voice-push-note v 1)
		 (assert-equal 1 current-voice-note))
	       (assert-nil (cl-synthesizer-midi-voice-manager::voice-remove-note v 1))))

