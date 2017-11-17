(in-package :cl-synthesizer-test)

(define-test test-sine-1 ()
	     "Test the sine-core by counting the frequency of the generated signal using 
              the trigger component.
              The test assumes that the trigger component has been tested :)"
	     (let ((trigger (funcall #'cl-synthesizer-core:trigger :switching-voltage 5))
		   (sine (funcall #'cl-synthesizer-core:sine-core :f-min 10 :f-max 100 :v-peak 5 :sample-rate 1000))
		   (trigger-count 0))
	       (dotimes (i 7000)
		 (if (funcall (getf trigger :is-firing) (funcall (getf sine :tick) 1))
		     (setf trigger-count (+ trigger-count 1))))
	       (assert-equal 7 trigger-count)))
