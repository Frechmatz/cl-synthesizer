(in-package :cl-synthesizer-core)

;; todo: think about clipping
;; todo: implement reset function
(defun sine-core (&key f-min f-max v-peak sample-rate)
  "Implements a sine generator with a given frequency range.
   Returns two functions: 
   - tick (frequency)
   - reset ()"
  (declare (ignore f-min f-max))
  (let ((generator (phase-generator sample-rate)))
    (list
     :tick (lambda (frequency)
	     (* v-peak (sin (funcall generator frequency))))
     :reset (lambda () nil))))
