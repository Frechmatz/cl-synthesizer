(in-package :cl-synthesizer-test)

(defun frequency-counter (&key sample-rate update-fn get-output-fn)
  "Determines the frequency in Hz of a signal by counting the zero-crossings."
  (let ((seconds 2)
	(trigger (zero-crossing-trigger))
	(zero-crossing-count 0))
    (dotimes (i (* seconds sample-rate))
      (funcall update-fn)
      (if (funcall trigger (funcall get-output-fn))
	  (setf zero-crossing-count (+ 1 zero-crossing-count))))
    (float (/ zero-crossing-count (* seconds 2)))))

