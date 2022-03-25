(in-package :cl-synthesizer-test)

;;
;; Lru-Set
;;

(defun run-lru-set-test-case (test-case)
  (let ((mgr (make-instance 'cl-synthesizer-midi-lru-set::lru-set :capacity (getf test-case :capacity))))
    (dolist (test-case (getf test-case :test-cases))
      (let ((cmd (first test-case))
	    (cmd-arg (second test-case))
	    (expected-entry-index (getf test-case :expected-entry-index))
	    (expected-current-value (getf test-case :expected-current-value))
	    (expected-stolen (getf test-case :expected-stolen))
	    (expected-entry-count (getf test-case :expected-entry-count)))
	(cond
	  ((eq cmd :push)
	   (multiple-value-bind (resulting-index stolen)
	       (cl-synthesizer-midi-lru-set::push-value mgr cmd-arg)
	     (assert-equal expected-entry-index resulting-index)
	     (assert-equal expected-stolen stolen)
	     (if expected-entry-count
		 (assert-equal expected-entry-count (cl-synthesizer-midi-lru-set::entry-count mgr)))
	     (let ((current-value (cl-synthesizer-midi-lru-set::current-value mgr)))
	       (assert-equal expected-current-value current-value))))
	  ((eq cmd :remove)
	   (let ((resulting-index (cl-synthesizer-midi-lru-set::remove-value mgr cmd-arg)))
	     (assert-equal expected-entry-index resulting-index))
	   (let ((current-value (cl-synthesizer-midi-lru-set::current-value mgr)))
	     (assert-equal expected-current-value current-value))
	   (if expected-entry-count
	       (progn 
		 (assert-equal expected-entry-count (cl-synthesizer-midi-lru-set::entry-count mgr))
		 (if (= 0 expected-entry-count)
		     (assert-true (cl-synthesizer-midi-lru-set::empty-p mgr))))))
	  (t
	   (error "Invalid test case")))))))

(define-test test-lru-set-1 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A" :expected-entry-count 1)
		       (:push "B" :expected-entry-index 1 :expected-current-value "B" :expected-entry-count 2)
		       (:remove "A" :expected-entry-index 0 :expected-current-value "B" :expected-entry-count 1)
		       (:push "C" :expected-entry-index 0 :expected-current-value "C" :expected-entry-count 2)
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
		       (:push "B" :expected-entry-index 1 :expected-current-value "B" :expected-entry-count 2)
		       (:remove "B" :expected-entry-index 1 :expected-current-value "A" :expected-entry-count 1)
		       (:remove "A" :expected-entry-index 0 :expected-current-value nil :expected-entry-count 0)
		       (:push "C" :expected-entry-index 1 :expected-current-value "C")))))
	     (run-lru-set-test-case test)))

(define-test test-lru-set-3 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A" :expected-entry-count 1)
		       (:push "B" :expected-entry-index 1 :expected-current-value "B" :expected-entry-count 2)
		       (:push "C" :expected-entry-index 0 :expected-current-value "C" :expected-stolen t :expected-entry-count 2)
		       (:push "D" :expected-entry-index 1 :expected-current-value "D" :expected-stolen t :expected-entry-count 2)
		       (:push "E" :expected-entry-index 0 :expected-current-value "E" :expected-stolen t :expected-entry-count 2)
		       (:push "F" :expected-entry-index 1 :expected-current-value "F" :expected-stolen t :expected-entry-count 2)))))
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

;; push duplicates
(define-test test-lru-set-5 ()
	     (let ((test
		    '(:capacity 4
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A" :expected-entry-count 1)
		       ;; Keep index
		       (:push "A" :expected-entry-index 0 :expected-current-value "A" :expected-entry-count 1)
		       (:remove "A" :expected-entry-index 0 :expected-current-value nil :expected-entry-count 0)
		       ;; do not assign to index 0 but to 1
		       (:push "C" :expected-entry-index 1 :expected-current-value "C" :expected-entry-count 1)))))
	     (run-lru-set-test-case test)))

