(in-package :cl-synthesizer-core)

;; todo: think about clipping
;; todo: implement reset function
(defun triangle-core (&key f-min f-max sample-rate)
  "Implements a triangle generator with a given frequency range.
   Returns two functions: 
   - tick (frequency)
   - reset ()"
  (declare (ignore f-min f-max))
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((generator (phase-generator sample-rate)))
    (list
     :tick (lambda (frequency)
	     (declare (optimize (debug 3) (speed 0) (space 0)))
	     ;; 0...PI...2*PI -> 1...-1...1 
	     (let ((phi (funcall generator frequency)))
	       (let ((normalized (/ phi PI))) ;; 0..2
		 (let ((y (+ -1 (* 2 (abs (+ -1 normalized))))))
		   y
		 ))))
     :reset (lambda () nil))))
