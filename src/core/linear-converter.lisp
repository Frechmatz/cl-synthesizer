(in-package :cl-synthesizer-core)

;; todo: frequency clipping
(defun linear-converter (&key cv-min cv-max f-min f-max)
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

