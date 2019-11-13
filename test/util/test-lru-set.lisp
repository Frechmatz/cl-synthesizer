(in-package :cl-synthesizer-test)

;;
;; Lru-Set
;;

(defun run-lru-set-test-case (test-case)
  (let ((mgr (make-instance 'cl-synthesizer-lru-set:lru-set :capacity (getf test-case :capacity))))
    (dolist (test-case (getf test-case :test-cases))
      (let ((cmd (first test-case))
	    (cmd-arg (second test-case))
	    (expected-entry-index (getf test-case :expected-entry-index))
	    (expected-current-value (getf test-case :expected-current-value)))
	(cond
	  ((eq cmd :push)
	   (let ((resulting-index (cl-synthesizer-lru-set:push-value mgr cmd-arg)))
	     (assert-equal expected-entry-index resulting-index))
	   (let ((current-value (cl-synthesizer-lru-set:current-value mgr)))
	     (assert-equal expected-current-value current-value)))
	  ((eq cmd :remove)
	   (let ((resulting-index (cl-synthesizer-lru-set:remove-value mgr cmd-arg)))
	     (assert-equal expected-entry-index resulting-index))
	   (let ((current-value (cl-synthesizer-lru-set:current-value mgr)))
	     (assert-equal expected-current-value current-value)))
	  (t
	   (error "Invalid test case")))))))

(define-test test-lru-set-1 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")
		       (:remove "A" :expected-entry-index 0 :expected-current-value "B")
		       (:push "C" :expected-entry-index 0 :expected-current-value "C")
		       (:remove "B" :expected-entry-index 1 :expected-current-value "C")
		       (:push "D" :expected-entry-index 1 :expected-current-value "D")
		       (:remove "D" :expected-entry-index 1 :expected-current-value "C")
		       (:push "D" :expected-entry-index 1 :expected-current-value "D")
		       (:remove "D" :expected-entry-index 1 :expected-current-value "C")))))
	     (run-lru-set-test-case test)))

;; Tests that when a new entry is added, that it is assigned to the least recently used index
(define-test test-lru-set-2 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")
		       (:remove "B" :expected-entry-index 1 :expected-current-value "A")
		       (:remove "A" :expected-entry-index 0 :expected-current-value nil)
		       (:push "C" :expected-entry-index 1 :expected-current-value "C")))))
	     (run-lru-set-test-case test)))

(define-test test-lru-set-3 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")
		       (:push "C" :expected-entry-index 0 :expected-current-value "C")
		       (:push "D" :expected-entry-index 1 :expected-current-value "D")
		       (:push "E" :expected-entry-index 0 :expected-current-value "E")
		       (:push "F" :expected-entry-index 1 :expected-current-value "F")))))
	     (run-lru-set-test-case test)))

(define-test test-lru-set-4 ()
	     (let ((test
		    '(:capacity 4
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")
		       (:remove "B" :expected-entry-index 1 :expected-current-value "A")
		       ;; do not assign to index 1 (even if it is available) but to 2
		       (:push "D" :expected-entry-index 2 :expected-current-value "D")))))
	     (run-lru-set-test-case test)))

;; push same value multiple times
(define-test test-lru-set-5 ()
	     (let ((test
		    '(:capacity 4
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       ;; Keep index
		       (:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:remove "A" :expected-entry-index 0 :expected-current-value nil)
		       ;; do not assign to index 0 but to 1
		       (:push "C" :expected-entry-index 1 :expected-current-value "C")))))
	     (run-lru-set-test-case test)))


