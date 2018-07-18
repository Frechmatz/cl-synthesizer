(in-package :cl-synthesizer-test)

;;
;; voice-manager tests
;;


(defun run-test-case (test-case)
  (let ((mgr (make-instance 'cl-synthesizer-midi-voice-manager::voice-manager :voice-count (getf test-case :voice-count))))
    (dolist (test-case (getf test-case :test-cases))
      (let ((cmd (first test-case))
	    (cmd-arg (second test-case))
	    (expected-voice-number (getf test-case :expected-voice-number))
	    (expected-note (getf test-case :expected-note))
	    )
	(format t "Expected Number: ~a Expected Note: ~a~%"
		expected-voice-number expected-note)
	(multiple-value-bind (resulting-voice-number resulting-note)
	    (if (eq cmd :push)
		(cl-synthesizer-midi-voice-manager:push-note mgr cmd-arg)
		(if (eq cmd :remove)
		    (cl-synthesizer-midi-voice-manager:remove-note mgr cmd-arg)
		    (error "STOP")))
	  (format t "Number: ~a Note: ~a~%" resulting-voice-number resulting-note)
	  (assert-equal expected-voice-number resulting-voice-number)
	  (assert-equal expected-note resulting-note))))))


;;
;;
;; Tests, where no voice overloading takes place
;;

;; test that A and B are allocated to voices 0 and 1
(define-test test-voice-manager-mgr-no-overload-0 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A")
		       (:push "B" :expected-voice-number 1 :expected-note "B")))))
	     (run-test-case test)))

;; test that A is removed from voice 0
(define-test test-voice-manager-mgr-no-overload-1 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A")
		       (:push "B" :expected-voice-number 1 :expected-note "B")
		       (:remove "A" :expected-voice-number 0 :expected-note nil)
		       ))))
	     (run-test-case test)))

;; Test that C is allocated to previously released slot of A
(define-test test-voice-manager-mgr-no-overload-2 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A")
		       (:push "B" :expected-voice-number 1 :expected-note "B")
		       (:remove "A" :expected-voice-number 0 :expected-note nil)
		       (:push "C" :expected-voice-number 0 :expected-note "C")
		       ))))
	     (run-test-case test)))

;; Test that C is allocated to previously released slot of B
(define-test test-voice-manager-mgr-no-overload-3 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A")
		       (:push "B" :expected-voice-number 1 :expected-note "B")
		       (:remove "B" :expected-voice-number 1 :expected-note nil)
		       (:push "C" :expected-voice-number 1 :expected-note "C")
		       ))))
	     (run-test-case test)))

;; Test that A (voice 0) is playing after multiple adds/removes to voice 1
(define-test test-voice-manager-mgr-no-overload-4 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A")
		       (:push "B" :expected-voice-number 1 :expected-note "B")
		       (:remove "B" :expected-voice-number 1 :expected-note nil)
		       (:push "C" :expected-voice-number 1 :expected-note "C")
		       (:remove "C" :expected-voice-number 1 :expected-note nil)
		       (:push "D" :expected-voice-number 1 :expected-note "D")
		       ))))
	     (run-test-case test)))

(define-test test-voice-manager-mgr-no-overload-5 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A")
		       (:push "B" :expected-voice-number 1 :expected-note "B")
		       (:remove "A" :expected-voice-number 0 :expected-note nil)
		       (:push "C" :expected-voice-number 0 :expected-note "C")

		       ))))
	     (run-test-case test)))

;; both voices played and released, next note will be assigned to first released voice
(define-test test-voice-manager-mgr-no-overload-6 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A")
		       (:push "B" :expected-voice-number 1 :expected-note "B")
		       (:remove "A" :expected-voice-number 0 :expected-note nil)
		       (:remove "B" :expected-voice-number 1 :expected-note nil)
		       (:push "C" :expected-voice-number 0 :expected-note "C")
		       ))))
	     (run-test-case test)))

;; both voices played and released, next note will be assigned to first released voice
(define-test test-voice-manager-mgr-no-overload-7 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A")
		       (:push "B" :expected-voice-number 1 :expected-note "B")
		       (:remove "B" :expected-voice-number 1 :expected-note nil)
		       (:remove "A" :expected-voice-number 0 :expected-note nil)
		       (:push "C" :expected-voice-number 1 :expected-note "C")
		       ))))
	     (run-test-case test)))

;;
;;
;; Tests, where voice overloading takes place
;; Test cases where all voices are occupied and new notes are to be pushed
;;
;;

(define-test test-voice-manager-mgr-overload-0 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A")
		       (:push "B" :expected-voice-number 1 :expected-note "B")
		       (:push "C" :expected-voice-number 0 :expected-note "C")
		       (:push "D" :expected-voice-number 1 :expected-note "D")
		       (:push "E" :expected-voice-number 0 :expected-note "E")
		       (:push "F" :expected-voice-number 1 :expected-note "F")
		       ))))
	     (run-test-case test)))

;; Fast ON/OFF of notes when all voices are playing. Current note should be consecutively assigned to same voice index
(define-test test-voice-manager-mgr-overload-1 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A")
		       (:push "B" :expected-voice-number 1 :expected-note "B")
		       (:push "C-ON" :expected-voice-number 0 :expected-note "C-ON")
		       (:remove "C-ON" :expected-voice-number 0 :expected-note nil)
		       (:push "D-ON" :expected-voice-number 0 :expected-note "D-ON")
		       ))))
	     (run-test-case test)))

;; Fast ON/OFF of notes when all voices are playing. Current note should be consecutively assigned to same voice index
(define-test test-voice-manager-mgr-overload-2 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A")
		       (:push "B" :expected-voice-number 1 :expected-note "B")
		       (:push "C" :expected-voice-number 0 :expected-note "C")
		       (:push "D-ON" :expected-voice-number 1 :expected-note "D-ON")
		       (:remove "D-ON" :expected-voice-number 1 :expected-note nil)
		       (:push "D-ON" :expected-voice-number 1 :expected-note "D-ON")
		       ))))
	     (run-test-case test)))

(define-test test-voice-manager-mgr-overload-3 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A")
		       (:push "B" :expected-voice-number 1 :expected-note "B")
		       (:push "C" :expected-voice-number 0 :expected-note "C")
		       (:remove "B" :expected-voice-number 1 :expected-note nil)
		       ))))
	     (run-test-case test)))

