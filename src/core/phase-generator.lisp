(in-package :cl-synthesizer-core)

(defconstant 2PI (coerce (* 2 PI) 'single-float))

(defun phase-generator (sample-rate)
  "Generates a phase 0 <= Phi < 2PI depending on frequency and sample-rate
   Returns a values object containing two functions: 
   - lambda(frequency): A function that returns the phase according to the frequency.
     If the frequency is negative then phi will move in clockwise (backward) direction.
   - lambda(): Resets the phase to 0.0"
  (declare (type single-float sample-rate))
  (let ((phi 0.0))
    (flet ((get-delta-phi (frequency)
	     (/ (* 2PI frequency) sample-rate)))
      (values
       (lambda (frequency)
	 (declare (type single-float frequency))
	 (setf phi (rem (+ phi (get-delta-phi frequency)) 2PI))
	 (if (> 0.0 phi)
	     (+ 2PI phi) ;; -45 deg => 315 deg
	     phi))
       (lambda ()
	 (setf phi 0.0)
	 phi)))))

