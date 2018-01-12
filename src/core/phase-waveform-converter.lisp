;;
;; Waveform generators
;; The generators convert a phase to an amplitude.
;; As a "rule of thumb" the amplitudes are aligned
;; with the amplitude of sine.
;;

(in-package :cl-synthesizer-core)

(defun phase-sine-converter (phi &key (phase-offset 0))
  "phi -- The phase in radiant. 0...POSITIVE-INFINITY
   phase-offset -- An optional phase offset in radians. Must be greater or equal zero"
  (sin (+ phi phase-offset)))

(alexandria:define-constant +SAW-OFFSET+ PI
  :documentation "Phase offset of saw-converter to align with amplitude of sine")
(defun phase-saw-converter (phi &key (phase-offset 0))
  "phi -- The phase in radiant. 0...POSITIVE-INFINITY
   phase-offset -- An optional phase offset in radians. Must be greater or equal zero"
  (let ((normalized (/ (mod (+ phi +SAW-OFFSET+ phase-offset) (* 2 PI)) PI))) ;; 0..2
    (+ -1 (mod normalized 2))))

(defun phase-square-converter (phi &key (duty-cycle 0.5) (phase-offset 0))
  "phi -- The phase in radiant. 0...POSITIVE-INFINITY
   duty-cycle -- An optional duty-cycle. The default value is 0.5. 0 >= duty-cycle <= 1
   phase-offset -- An optional phase offset in radians. Must be greater or equal zero"
  (let ((y (if (< (mod (+ phi phase-offset) (* 2 PI)) (* 2 PI duty-cycle)) 1 -1)))
    y))

(alexandria:define-constant +TRIANGLE-OFFSET+ (* 0.75 2 PI)
  :documentation "Phase offset of triangle-converter to align with amplitude of sine")
(defun phase-triangle-converter (phi &key (phase-offset 0))
  "phi -- The phase in radiant. 0...POSITIVE-INFINITY
   phase-offset -- An optional phase offset in radians. Must be greater or equal zero"
  (let ((normalized (/ (mod (+ phi +TRIANGLE-OFFSET+ phase-offset) (* 2 PI)) PI))) ;; 0..2
    (let ((y (+ -1 (* 2 (abs (+ -1 normalized))))))
      y)))
