(in-package :cl-synthesizer-test)

(define-test test-saw-1 ()
	     "Test the saw-core by counting the zero-crossings of the signal"
	     (let ((trigger (funcall #'cl-synthesizer-test::zero-crossing-trigger))
		   (saw (funcall #'cl-synthesizer-core:saw-core :f-min 10 :f-max 100 :sample-rate 1000))
		   (trigger-count 0))
	       (dotimes (i 7250)
		 (if (funcall trigger (funcall (getf saw :tick) 1))
		     (setf trigger-count (+ trigger-count 1))))
	       ;; 14 zero-crossings
	       (assert-equal 14 trigger-count)))
