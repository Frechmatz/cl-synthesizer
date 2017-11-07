;;
;; A VCO supporting multiple wave forms
;;
;; Work in progress
;;

(in-package :cl-synthesizer-modules-vco)

(defun transfer-function-linear (&key cv-min cv-max f-min f-max)
  "Defines a linear transfer function. Returns two functions:
   control-voltage -> frequency
   frequency -> control-voltage"
  (let ((m (/ (- f-max f-min) (- cv-max cv-min)))) ;; deltaY / deltaX
    (list
     :get-frequency ;; x -> y
     (lambda (cv)
       ;; y = y0 + m * (x - x0)
       (+ f-min (* m (- cv cv-min))))
     :get-cv ;; y -> x
     (lambda (frequency)
       ;; x = ((y - y0) + (m * x0)) / m
       (/ (+ (- frequency f-min) (* m cv-min)) m)))))

#|
(defparameter *fn* (transfer-function-linear :cv-min -3.0 :cv-max 6.0 :f-min 0 :f-max 5))
(funcall (getf *fn* :get-frequency) 0)
(funcall (getf *fn* :get-cv) 5)
(funcall (getf *fn* :get-cv) 2.5)
|#

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

(defun sine-core (&key f-min f-max v-peak sample-rate)
  "Implements a sine generator with a given frequency range.
   Returns two functions: 
   - tick (frequency)
   - reset ()"
  (declare (ignore f-min f-max))
  (let ((generator (phase-generator sample-rate)))
    (list
     :tick (lambda (frequency)
	     ;; todo: Frequency clipping
	     (* v-peak (sin (funcall generator frequency))))
     :reset (lambda () nil))))


(defun triangle-core (&key f-min f-max v-peak sample-rate)
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
	     ;; todo: Frequency clipping
	     ;; 0...PI...2*PI -> 1...-1...1 
	     (let ((phi (funcall generator frequency)))
	       (let ((normalized (/ phi PI))) ;; 0..2
		 (let ((y (+ -1 (* 2 (abs (+ -1 normalized))))))
		   (* v-peak y)
		 ))))
     :reset (lambda () nil))))

(defun vco (environment &key (f-0 440) (cv-min -5) (cv-max 5) (f-min 0) (f-max 12000) (v-peak 5))
  (let* ((sample-rate (getf environment :sample-rate))
	 (transfer-function
	  (transfer-function-linear :cv-min cv-min :cv-max cv-max :f-min f-min :f-max f-max))
	 (sine-vco
	  (sine-core :f-min f-min :f-max f-max :v-peak v-peak :sample-rate sample-rate))
	 (triangle-vco
	  (triangle-core :f-min f-min :f-max f-max :v-peak v-peak :sample-rate sample-rate))
	 (inputs (list :cv))
	 (outputs (list :sine :triangle))
	 (cur-sine-output 1.0)
	 (cur-triangle-output 1.0)
	 (cv-offs (funcall (getf transfer-function :get-cv) f-0)))
    (flet ((get-frequency (cv)
	     (funcall (getf transfer-function :get-frequency) (+ cv cv-offs))))
      (list
       :shutdown (lambda () nil)
       :inputs (lambda () inputs)
       :outputs (lambda () outputs)
       :get-output (lambda (output)
		     (cond
		       ((eq output :sine) cur-sine-output)
		       ((eq output :triangle) cur-triangle-output)
		       (t (error (format nil "Unknown input ~a requested from VCO" output)))))
       :update (lambda (&key (cv 0))
		 (let ((f (get-frequency cv)))
		   (setf cur-sine-output (funcall (getf sine-vco :tick) f))
		   (setf cur-triangle-output (funcall (getf triangle-vco :tick) f))
		   ))))))

