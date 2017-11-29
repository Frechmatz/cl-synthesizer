(in-package :cl-synthesizer-test)

(define-test test-square-1 ()
	     "Test the square-core by counting the zero-crossings of the signal"
	     (let ((trigger (funcall #'cl-synthesizer-test::zero-crossing-trigger))
		   (square (funcall #'cl-synthesizer-core:square-core :f-min 10 :f-max 100 :sample-rate 1000))
		   (trigger-count 0))
	       (dotimes (i 7250)
		 (if (funcall trigger (funcall (getf square :tick) 1))
		     (setf trigger-count (+ trigger-count 1))))
	       ;; 14 zero-crossings
	       (assert-equal 14 trigger-count)))
