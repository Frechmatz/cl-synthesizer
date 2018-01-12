(in-package :cl-synthesizer-core)

(defun linear-converter (&key input-min input-max output-min output-max)
  "Defines linear transfer functions. Returns two functions:
   input-to-output -> Can be used for example to convert a control voltage to a frequency
   output-to-input -> Can be used for example to convert a frequency to a control voltage"
  (let ((m (/ (- output-max output-min) (- input-max input-min)))) ;; deltaY / deltaX
    (list
     :input-to-output ;; x -> y
     (lambda (input-value)
       ;; y = y0 + m * (x - x0)
       (+ output-min (* m (- input-value input-min))))
     :output-to-input ;; y -> x
     (lambda (output-value)
       ;; x = ((y - y0) + (m * x0)) / m
       (/ (+ (- output-value output-min) (* m input-min)) m)))))

