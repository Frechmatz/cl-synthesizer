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
	    (expected-stack-size (getf test-case :expected-stack-size)))
	(format t "Expected Number: ~a Expected Note: ~a Expected Stacksize: ~a~%"
		expected-voice-number expected-note expected-stack-size)
	(multiple-value-bind (resulting-voice-number resulting-note resulting-stack-size)
	    (if (eq cmd :push)
		(cl-synthesizer-midi-voice-manager:push-note mgr cmd-arg)
		(if (eq cmd :remove)
		    (cl-synthesizer-midi-voice-manager:remove-note mgr cmd-arg)
		    (error "STOP")))
	  (format t "Number: ~a Note: ~a Stacksize: ~a~%" resulting-voice-number resulting-note resulting-stack-size)
	  (assert-equal expected-voice-number resulting-voice-number)
	  (assert-equal expected-note resulting-note)
	  (assert-equal expected-stack-size resulting-stack-size))))))


;;
;;
;; Tests, where no voice overloading takes place
;;

;; test that A and B are allocated to voices 0 and 1
(define-test test-voice-manager-mgr-no-overload-0 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A" :expected-stack-size 1)
		       (:push "B" :expected-voice-number 1 :expected-note "B" :expected-stack-size 1)))))
	     (run-test-case test)))

;; test that A is removed from voice 0
(define-test test-voice-manager-mgr-no-overload-1 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A" :expected-stack-size 1)
		       (:push "B" :expected-voice-number 1 :expected-note "B" :expected-stack-size 1)
		       (:remove "A" :expected-voice-number 0 :expected-note nil :expected-stack-size 0)
		       ))))
	     (run-test-case test)))

;; Test that C is allocated to previously released slot of A
(define-test test-voice-manager-mgr-no-overload-2 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A" :expected-stack-size 1)
		       (:push "B" :expected-voice-number 1 :expected-note "B" :expected-stack-size 1)
		       (:remove "A" :expected-voice-number 0 :expected-note nil :expected-stack-size 0)
		       (:push "C" :expected-voice-number 0 :expected-note "C" :expected-stack-size 1)
		       ))))
	     (run-test-case test)))

;; Test that C is allocated to previously released slot of B
(define-test test-voice-manager-mgr-no-overload-3 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A" :expected-stack-size 1)
		       (:push "B" :expected-voice-number 1 :expected-note "B" :expected-stack-size 1)
		       (:remove "B" :expected-voice-number 1 :expected-note nil :expected-stack-size 0)
		       (:push "C" :expected-voice-number 1 :expected-note "C" :expected-stack-size 1)
		       ))))
	     (run-test-case test)))

;; Test that A (voice 0) is playing after multiple adds/removes to voice 1
(define-test test-voice-manager-mgr-no-overload-4 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A" :expected-stack-size 1)
		       (:push "B" :expected-voice-number 1 :expected-note "B" :expected-stack-size 1)
		       (:remove "B" :expected-voice-number 1 :expected-note nil :expected-stack-size 0)
		       (:push "C" :expected-voice-number 1 :expected-note "C" :expected-stack-size 1)
		       (:remove "C" :expected-voice-number 1 :expected-note nil :expected-stack-size 0)
		       (:push "D" :expected-voice-number 1 :expected-note "D" :expected-stack-size 1)
		       ))))
	     (run-test-case test)))

(define-test test-voice-manager-mgr-no-overload-5 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A" :expected-stack-size 1)
		       (:push "B" :expected-voice-number 1 :expected-note "B" :expected-stack-size 1)
		       (:remove "A" :expected-voice-number 0 :expected-note nil :expected-stack-size 0)
		       (:push "C" :expected-voice-number 0 :expected-note "C" :expected-stack-size 1)

		       ))))
	     (run-test-case test)))

;; both voices played and released, next note will be assigned to first released voice
(define-test test-voice-manager-mgr-no-overload-6 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A" :expected-stack-size 1)
		       (:push "B" :expected-voice-number 1 :expected-note "B" :expected-stack-size 1)
		       (:remove "A" :expected-voice-number 0 :expected-note nil :expected-stack-size 0)
		       (:remove "B" :expected-voice-number 1 :expected-note nil :expected-stack-size 0)
		       (:push "C" :expected-voice-number 0 :expected-note "C" :expected-stack-size 1)
		       ))))
	     (run-test-case test)))

;; both voices played and released, next note will be assigned to first released voice
(define-test test-voice-manager-mgr-no-overload-7 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A" :expected-stack-size 1)
		       (:push "B" :expected-voice-number 1 :expected-note "B" :expected-stack-size 1)
		       (:remove "B" :expected-voice-number 1 :expected-note nil :expected-stack-size 0)
		       (:remove "A" :expected-voice-number 0 :expected-note nil :expected-stack-size 0)
		       (:push "C" :expected-voice-number 1 :expected-note "C" :expected-stack-size 1)
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
		      ((:push "A" :expected-voice-number 0 :expected-note "A" :expected-stack-size 1)
		       (:push "B" :expected-voice-number 1 :expected-note "B" :expected-stack-size 1)
		       (:push "C" :expected-voice-number 0 :expected-note "C" :expected-stack-size 2)
		       ))))
	     (run-test-case test)))

;; Arpeggio when all voices are playing. Current note should be consecutively assigned to same voice index
(define-test test-voice-manager-mgr-overload-1 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A" :expected-stack-size 1)
		       (:push "B" :expected-voice-number 1 :expected-note "B" :expected-stack-size 1)
		       (:push "ARP-C" :expected-voice-number 0 :expected-note "ARP-C" :expected-stack-size 2)
		       (:remove "ARP-C" :expected-voice-number 0 :expected-note "A" :expected-stack-size 1)
		       (:push "ARP-D" :expected-voice-number 0 :expected-note "ARP-D" :expected-stack-size 2)
		       ))))
	     (run-test-case test)))

;; Arpeggio when all voices are playing. Current note should be consecutively assigned to same voice index
(define-test test-voice-manager-mgr-overload-2 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-note "A" :expected-stack-size 1)
		       (:push "B" :expected-voice-number 1 :expected-note "B" :expected-stack-size 1)
		       (:push "C" :expected-voice-number 0 :expected-note "C" :expected-stack-size 2)
		       (:push "ARP-D" :expected-voice-number 1 :expected-note "ARP-D" :expected-stack-size 2)
		       ;;(:remove "ARP-D" :expected-voice-number 1 :expected-note "B" :expected-stack-size 1)
		       ;;(:push "ARP-D" :expected-voice-number 1 :expected-note "ARP-D" :expected-stack-size 2)
		       ))))
	     (run-test-case test)))

