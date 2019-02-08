(in-package :cl-synthesizer-test)

(defun output-change-counter (&key sample-rate duration-seconds update-fn get-output-fn )
  "Counts the number of value changes."
  (let ((first-call t) (cur-output-value nil)
	(change-count 0))
    (dotimes (i (* duration-seconds sample-rate))
      (funcall update-fn)
      (let ((output-value (funcall get-output-fn)))
	(if first-call
	    (setf first-call nil)
	    (progn
	      (if (not (= output-value cur-output-value))
		  (setf change-count (+ 1 change-count)))))
	(setf cur-output-value output-value)))
    change-count))
