(in-package :cl-synthesizer-test)

(define-test test-square-1 ()
	     "Test the square-core by counting the frequency of the generated signal using 
              the trigger component.
              The test assumes that the trigger component has been tested :)"
	     (let ((trigger (funcall #'cl-synthesizer-core:trigger :switching-voltage 5))
		   (square (funcall #'cl-synthesizer-core:square-core :f-min 10 :f-max 100 :v-peak 5 :sample-rate 1000))
		   (trigger-count 0))
	       (dotimes (i 7000)
		 (if (funcall (getf trigger :is-firing) (funcall (getf square :tick) 1))
		     (setf trigger-count (+ trigger-count 1))))
	       (assert-equal 7 trigger-count)))
