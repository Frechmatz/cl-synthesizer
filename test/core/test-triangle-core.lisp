(in-package :cl-synthesizer-test)

(define-test test-triangle-1 ()
	     "Test the triangle-core by counting the zero-crossings of the signal"
	     (let ((trigger (funcall #'cl-synthesizer-test::zero-crossing-trigger))
		   (triangle (funcall #'cl-synthesizer-core:triangle-core :f-min 10 :f-max 100 :v-peak 5 :sample-rate 1000))
		   (trigger-count 0))
	       (dotimes (i 7250)
		 (if (funcall trigger (funcall (getf triangle :tick) 1))
		     (setf trigger-count (+ trigger-count 1))))
	       ;; 14 zero-crossings
	       (assert-equal 14 trigger-count)))
