(in-package :cl-synthesizer-core)

(defun phase-generator (sample-rate)
  "Generates a phase 0 >= phi < 2pi depending on frequency and sample-rate
   Returns a function that has the following parameters:
   - frequency -- the current frequency
   The function returns the current phase."
  (let ((cur-phi 0))
    (flet ((get-delta-phi (frequency)
	     (let ((deltaPhi (/ (* 2 PI frequency) sample-rate)))
	       deltaPhi)))
      (lambda (frequency)
	(let ((r cur-phi))
	  (setf cur-phi (+ cur-phi (get-delta-phi frequency)))
	  (if (> cur-phi (* 2 PI))
	      ;; 'reset' cur-phi to avoid overflow and for maximum precision
	      (setf cur-phi (- cur-phi (* 2 PI))))
	  r)))))

