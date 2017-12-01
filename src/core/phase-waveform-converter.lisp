(in-package :cl-synthesizer-core)

(defun phase-sine-converter (phi)
  (sin phi))

(defun phase-saw-converter (phi)
  (let ((normalized (/ phi PI))) ;; 0..2
    (+ -1 (mod normalized 2))))

(defun phase-square-converter (phi)
  (let ((y (if (< phi PI) 1 -1)))
    y))

(defun phase-triangle-converter (phi)
  ;; 0...PI...2*PI -> 1...-1...1 
  (let ((normalized (/ phi PI))) ;; 0..2
    (let ((y (+ -1 (* 2 (abs (+ -1 normalized))))))
      y)))
