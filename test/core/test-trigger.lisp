(in-package :cl-synthesizer-test)

(define-test test-trigger-1 ()
	     (let ((trigger (funcall #'cl-synthesizer-core:trigger :switching-voltage 5)))
	       (let ((is-firing-fn (getf trigger :is-firing)))
		 (assert-false (funcall is-firing-fn 0))
		 (assert-false (funcall is-firing-fn 2))
		 (assert-true (funcall is-firing-fn 5))
		 (assert-false (funcall is-firing-fn 6))
		 (assert-false (funcall is-firing-fn 5))
		 (assert-false (funcall is-firing-fn 4))
		 (assert-true (funcall is-firing-fn 5))
		 (assert-false (funcall is-firing-fn 6))
		 (assert-false (funcall is-firing-fn -6))
		 (assert-false (funcall is-firing-fn 4))
		 (assert-true (funcall is-firing-fn 5)))))


