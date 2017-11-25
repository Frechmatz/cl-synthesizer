(in-package :cl-synthesizer-test)

(define-test test-sine-1 ()
	     "Test the sine-core by counting the zero-crossings of the signal"
	     (let ((trigger (funcall #'cl-synthesizer-test::zero-crossing-trigger))
		   (sine (funcall #'cl-synthesizer-core:sine-core :f-min 10 :f-max 100 :v-peak 5 :sample-rate 1000))
		   (trigger-count 0))
	       (dotimes (i 7250)
		 (if (funcall trigger (funcall (getf sine :tick) 1))
		     (setf trigger-count (+ trigger-count 1))))
	       (assert-equal 14 trigger-count)))
