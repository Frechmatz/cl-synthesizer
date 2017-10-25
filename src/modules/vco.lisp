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

(defun sine-vco-component (&key f-min f-max v-peak sample-rate)
  "Implements a sine generator with a given frequency range.
   Returns a function with the following parameters:
   - frequency: The current frequency"
  (declare (ignore f-min f-max))
  (let ((cur-phi 0)
	(current-output nil))
    (flet ((get-delta-phi (frequency)
	     (let ((deltaPhi (/ (* 2 PI frequency) sample-rate)))
	       deltaPhi)))
      (lambda (frequency)
	(let ((deltaPhi (get-delta-phi frequency)))
	  ;; todo: Frequency clipping
	  (setf current-output (* (sin cur-phi) v-peak))
	  (setf cur-phi (+ cur-phi deltaPhi)))
	current-output))))

(defun vco (environment &key (f-0 440) (cv-min -5) (cv-max 5) (f-min 0) (f-max 12000) (v-peak 5))
  (let* ((sample-rate (getf environment :sample-rate))
	 (transfer-function
	  (transfer-function-linear :cv-min cv-min :cv-max cv-max :f-min f-min :f-max f-max))
	 (sine-vco
	  (sine-vco-component :f-min f-min :f-max f-max :v-peak v-peak :sample-rate sample-rate))
	 (inputs (list :cv))
	 (outputs (list :sine))
	 (cur-sine-output 1.0))
    (flet ((get-frequency (cv)
	     (let ((cv-offs (funcall (getf transfer-function :get-cv) f-0)))
	       (funcall (getf transfer-function :get-frequency) (+ cv cv-offs)))))
      (list
       :shutdown
       (lambda () nil)
       :inputs
       (lambda () inputs)
       :outputs
       (lambda () outputs)
       :get-output
       (lambda (output)
	 (cond
	   ((eq output :sine) cur-sine-output)
	   (t (error (format nil "Unknown input ~a requested from VCO" output)))))
       :update
       (lambda (&key (cv 0))
	 (let ((f (get-frequency cv)))
	   (setf cur-sine-output (funcall sine-vco f))))))))

