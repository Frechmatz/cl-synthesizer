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

;; test that A and B are allocated to indexes 0 and 1
(define-test test-lru-set-no-overload-0 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")))))
	     (run-lru-set-test-case test)))

;; test that A is removed from index 0
(define-test test-lru-set-no-overload-1 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")
		       (:remove "A" :expected-entry-index 0 :expected-current-value "B")
		       ))))
	     (run-lru-set-test-case test)))

;; Test that C is allocated to previously released slot of A
(define-test test-lru-set-no-overload-2 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")
		       (:remove "A" :expected-entry-index 0 :expected-current-value "B")
		       (:push "C" :expected-entry-index 0 :expected-current-value "C")
		       ))))
	     (run-lru-set-test-case test)))

;; Test that C is allocated to previously released slot of B
(define-test test-lru-set-no-overload-3 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")
		       (:remove "B" :expected-entry-index 1 :expected-current-value "A")
		       (:push "C" :expected-entry-index 1 :expected-current-value "C")
		       ))))
	     (run-lru-set-test-case test)))

;; Test that A (index 0) is playing after multiple adds/removes to index 1
(define-test test-lru-set-no-overload-4 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")
		       (:remove "B" :expected-entry-index 1 :expected-current-value "A")
		       (:push "C" :expected-entry-index 1 :expected-current-value "C")
		       (:remove "C" :expected-entry-index 1 :expected-current-value "A")
		       (:push "D" :expected-entry-index 1 :expected-current-value "D")
		       ))))
	     (run-lru-set-test-case test)))

(define-test test-lru-set-no-overload-5 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")
		       (:remove "A" :expected-entry-index 0 :expected-current-value "B")
		       (:push "C" :expected-entry-index 0 :expected-current-value "C")

		       ))))
	     (run-lru-set-test-case test)))

;; both indexes set and released, next value will be assigned to first released index
(define-test test-lru-set-no-overload-6 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")
		       (:remove "A" :expected-entry-index 0 :expected-current-value "B")
		       (:remove "B" :expected-entry-index 1 :expected-current-value nil)
		       (:push "C" :expected-entry-index 0 :expected-current-value "C")
		       ))))
	     (run-lru-set-test-case test)))

;; both indexes played and released, next value will be assigned to first released index
(define-test test-lru-set-no-overload-7 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")
		       (:remove "B" :expected-entry-index 1 :expected-current-value "A")
		       (:remove "A" :expected-entry-index 0 :expected-current-value nil)
		       (:push "C" :expected-entry-index 1 :expected-current-value "C")
		       ))))
	     (run-lru-set-test-case test)))

(define-test test-lru-set-overload-0 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")
		       (:push "C" :expected-entry-index 0 :expected-current-value "C")
		       (:push "D" :expected-entry-index 1 :expected-current-value "D")
		       (:push "E" :expected-entry-index 0 :expected-current-value "E")
		       (:push "F" :expected-entry-index 1 :expected-current-value "F")
		       ))))
	     (run-lru-set-test-case test)))

;; Fast ON/OFF of values when all entries are occupied. Current value should be consecutively assigned to same entry index
(define-test test-lru-set-overload-1 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")
		       (:push "C-ON" :expected-entry-index 0 :expected-current-value "C-ON")
		       (:remove "C-ON" :expected-entry-index 0 :expected-current-value "B")
		       (:push "D-ON" :expected-entry-index 0 :expected-current-value "D-ON")
		       ))))
	     (run-lru-set-test-case test)))

;; Fast ON/OFF of values when all entries are playing. Current value should be consecutively assigned to same entry index
(define-test test-lru-set-overload-2 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")
		       (:push "C" :expected-entry-index 0 :expected-current-value "C")
		       (:push "D-ON" :expected-entry-index 1 :expected-current-value "D-ON")
		       (:remove "D-ON" :expected-entry-index 1 :expected-current-value "C")
		       (:push "D-ON" :expected-entry-index 1 :expected-current-value "D-ON")
		       ))))
	     (run-lru-set-test-case test)))

(define-test test-lru-set-overload-3 ()
	     (let ((test
		    '(:capacity 2
		      :test-cases
		      ((:push "A" :expected-entry-index 0 :expected-current-value "A")
		       (:push "B" :expected-entry-index 1 :expected-current-value "B")
		       (:push "C" :expected-entry-index 0 :expected-current-value "C")
		       (:remove "B" :expected-entry-index 1 :expected-current-value "C")
		       ))))
	     (run-lru-set-test-case test)))


