(in-package :cl-synthesizer-core)

(defun linear-converter (&key input-min input-max output-min output-max)
  "Defines linear transfer functions. Returns two functions:
   get-y -> x -> y
   get-x -> y -> x (inverse function)"
  (declare (type single-float input-min input-max output-min output-max))
  (let ((m (/ (- output-max output-min) (- input-max input-min)))) ;; deltaY / deltaX
    (list
     :get-y
     (lambda (input-value)
       ;; y = y0 + m * (x - x0)
       (declare (type single-float input-value))
       (+ output-min (* m (- input-value input-min))))
     :get-x
     (lambda (output-value)
       ;; x = ((y - y0) + (m * x0)) / m
       (declare (type single-float output-value))
       (/ (+ (- output-value output-min) (* m input-min)) m)))))

