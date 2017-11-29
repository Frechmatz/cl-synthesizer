(in-package :cl-synthesizer-core)

;; todo: think about clipping
;; todo: implement reset function
;; todo: Duty cycle
(defun square-core (&key f-min f-max sample-rate)
  "Implements a square generator with a given frequency range.
   Returns two functions: 
   - tick (frequency)
   - reset ()"
  (declare (ignore f-min f-max))
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((generator (phase-generator sample-rate)))
    (list
     :tick (lambda (frequency)
	     (declare (optimize (debug 3) (speed 0) (space 0)))
	     (let ((phi (funcall generator frequency)))
		 (let ((y (if (< phi PI) 1 -1)))
		   y
		 )))
     :reset (lambda () nil))))
