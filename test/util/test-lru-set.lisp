(in-package :cl-synthesizer-test)

;;
;; Lru-Set
;;

(defun run-lru-set-test-case (test-case)
  (let ((mgr (make-instance 'cl-synthesizer-lru-set:lru-set :voice-count (getf test-case :voice-count))))
    (dolist (test-case (getf test-case :test-cases))
      (let ((cmd (first test-case))
	    (cmd-arg (second test-case))
	    (expected-voice-number (getf test-case :expected-voice-number))
	    (expected-current-note (getf test-case :expected-current-note))
	    (expected-note (getf test-case :expected-note)))
	(cond
	  ((eq cmd :push)
	   (let ((resulting-voice-number (cl-synthesizer-lru-set:push-note mgr cmd-arg)))
	     (assert-equal expected-voice-number resulting-voice-number))
	   (let ((current-note (cl-synthesizer-lru-set:current-note mgr)))
	     (assert-equal expected-current-note current-note)))
	  ((eq cmd :remove)
	   (multiple-value-bind (resulting-voice-number resulting-note)
	       (cl-synthesizer-lru-set:remove-note mgr cmd-arg)
	     (assert-equal expected-voice-number resulting-voice-number)
	     (assert-equal expected-note resulting-note))
	   (let ((current-note (cl-synthesizer-lru-set:current-note mgr)))
	     (assert-equal expected-current-note current-note)))
	  (t
	   (error "Invalid test case")))))))

;;
;; Tests, where no voice overloading takes place
;;

;; test that A and B are allocated to voices 0 and 1
(define-test test-lru-set-no-overload-0 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-current-note "A")
		       (:push "B" :expected-voice-number 1 :expected-current-note "B")))))
	     (run-lru-set-test-case test)))

;; test that A is removed from voice 0
(define-test test-lru-set-no-overload-1 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-current-note "A")
		       (:push "B" :expected-voice-number 1 :expected-current-note "B")
		       (:remove "A" :expected-voice-number 0 :expected-note nil :expected-current-note "B")
		       ))))
	     (run-lru-set-test-case test)))

;; Test that C is allocated to previously released slot of A
(define-test test-lru-set-no-overload-2 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-current-note "A")
		       (:push "B" :expected-voice-number 1 :expected-current-note "B")
		       (:remove "A" :expected-voice-number 0 :expected-note nil :expected-current-note "B")
		       (:push "C" :expected-voice-number 0 :expected-current-note "C")
		       ))))
	     (run-lru-set-test-case test)))

;; Test that C is allocated to previously released slot of B
(define-test test-lru-set-no-overload-3 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-current-note "A")
		       (:push "B" :expected-voice-number 1 :expected-current-note "B")
		       (:remove "B" :expected-voice-number 1 :expected-note nil :expected-current-note "A")
		       (:push "C" :expected-voice-number 1 :expected-current-note "C")
		       ))))
	     (run-lru-set-test-case test)))

;; Test that A (voice 0) is playing after multiple adds/removes to voice 1
(define-test test-lru-set-no-overload-4 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-current-note "A")
		       (:push "B" :expected-voice-number 1 :expected-current-note "B")
		       (:remove "B" :expected-voice-number 1 :expected-note nil :expected-current-note "A")
		       (:push "C" :expected-voice-number 1 :expected-current-note "C")
		       (:remove "C" :expected-voice-number 1 :expected-note nil :expected-current-note "A")
		       (:push "D" :expected-voice-number 1 :expected-current-note "D")
		       ))))
	     (run-lru-set-test-case test)))

(define-test test-lru-set-no-overload-5 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-current-note "A")
		       (:push "B" :expected-voice-number 1 :expected-current-note "B")
		       (:remove "A" :expected-voice-number 0 :expected-note nil :expected-current-note "B")
		       (:push "C" :expected-voice-number 0 :expected-current-note "C")

		       ))))
	     (run-lru-set-test-case test)))

;; both voices played and released, next note will be assigned to first released voice
(define-test test-lru-set-no-overload-6 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-current-note "A")
		       (:push "B" :expected-voice-number 1 :expected-current-note "B")
		       (:remove "A" :expected-voice-number 0 :expected-note nil :expected-current-note "B")
		       (:remove "B" :expected-voice-number 1 :expected-note nil :expected-current-note nil)
		       (:push "C" :expected-voice-number 0 :expected-current-note "C")
		       ))))
	     (run-lru-set-test-case test)))

;; both voices played and released, next note will be assigned to first released voice
(define-test test-lru-set-no-overload-7 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-current-note "A")
		       (:push "B" :expected-voice-number 1 :expected-current-note "B")
		       (:remove "B" :expected-voice-number 1 :expected-note nil :expected-current-note "A")
		       (:remove "A" :expected-voice-number 0 :expected-note nil :expected-current-note nil)
		       (:push "C" :expected-voice-number 1 :expected-current-note "C")
		       ))))
	     (run-lru-set-test-case test)))

;;
;;
;; Tests, where voice overloading takes place
;; Test cases where all voices are occupied and new notes are to be pushed
;;
;;

(define-test test-lru-set-overload-0 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-current-note "A")
		       (:push "B" :expected-voice-number 1 :expected-current-note "B")
		       (:push "C" :expected-voice-number 0 :expected-current-note "C")
		       (:push "D" :expected-voice-number 1 :expected-current-note "D")
		       (:push "E" :expected-voice-number 0 :expected-current-note "E")
		       (:push "F" :expected-voice-number 1 :expected-current-note "F")
		       ))))
	     (run-lru-set-test-case test)))

;; Fast ON/OFF of notes when all voices are playing. Current note should be consecutively assigned to same voice index
(define-test test-lru-set-overload-1 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-current-note "A")
		       (:push "B" :expected-voice-number 1 :expected-current-note "B")
		       (:push "C-ON" :expected-voice-number 0 :expected-current-note "C-ON")
		       (:remove "C-ON" :expected-voice-number 0 :expected-current-note "B")
		       (:push "D-ON" :expected-voice-number 0 :expected-current-note "D-ON")
		       ))))
	     (run-lru-set-test-case test)))

;; Fast ON/OFF of notes when all voices are playing. Current note should be consecutively assigned to same voice index
(define-test test-lru-set-overload-2 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-current-note "A")
		       (:push "B" :expected-voice-number 1 :expected-current-note "B")
		       (:push "C" :expected-voice-number 0 :expected-current-note "C")
		       (:push "D-ON" :expected-voice-number 1 :expected-current-note "D-ON")
		       (:remove "D-ON" :expected-voice-number 1 :expected-note nil :expected-current-note "C")
		       (:push "D-ON" :expected-voice-number 1 :expected-current-note "D-ON")
		       ))))
	     (run-lru-set-test-case test)))

(define-test test-lru-set-overload-3 ()
	     (let ((test
		    '(:voice-count 2
		      :test-cases
		      ((:push "A" :expected-voice-number 0 :expected-current-note "A")
		       (:push "B" :expected-voice-number 1 :expected-current-note "B")
		       (:push "C" :expected-voice-number 0 :expected-current-note "C")
		       (:remove "B" :expected-voice-number 1 :expected-note nil :expected-current-note "C")
		       ))))
	     (run-lru-set-test-case test)))


