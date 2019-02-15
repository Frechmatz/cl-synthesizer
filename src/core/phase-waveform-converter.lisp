;;
;; Waveform generators
;; The generators convert a phase to an amplitude.
;; As a "rule of thumb" the amplitudes are aligned
;; with the amplitude of sine.
;;

(in-package :cl-synthesizer-core)

(defconstant +SHORT-PI+ (coerce PI 'single-float))

(declaim (inline phase-sine-converter))
(defun phase-sine-converter (phi &key (phase-offset 0.0))
  "phi -- The phase in radiant. 0...POSITIVE-INFINITY
   phase-offset -- An optional phase offset in radians. Must be greater or equal zero"
  (declare (type single-float phi phase-offset))
  (sin (+ phi phase-offset)))

;; Phase offset of saw-converter to align with amplitude of sine
(defconstant +SAW-OFFSET+ (coerce PI 'single-float))

(declaim (inline phase-saw-converter))
(defun phase-saw-converter (phi &key (phase-offset 0.0))
  "phi -- The phase in radiant. 0...POSITIVE-INFINITY
   phase-offset -- An optional phase offset in radians. Must be greater or equal zero"
  (declare (type single-float phi phase-offset))
  (let ((normalized (/ (mod (+ phi +SAW-OFFSET+ phase-offset) (* 2.0 +SHORT-PI+)) +SHORT-PI+))) ;; 0..2
    (+ -1.0 (mod normalized 2.0))))

(declaim (inline phase-square-converter))
(defun phase-square-converter (phi &key (duty-cycle 0.5) (phase-offset 0.0))
  "phi -- The phase in radiant. 0...POSITIVE-INFINITY
   duty-cycle -- An optional duty-cycle. The default value is 0.5. 0 >= duty-cycle <= 1
   phase-offset -- An optional phase offset in radians. Must be greater or equal zero"
  (declare (type single-float phi phase-offset duty-cycle))
  (let ((y (if (< (mod (+ phi phase-offset) (* 2.0 +SHORT-PI+)) (* 2.0 +SHORT-PI+ duty-cycle)) 1.0 -1.0)))
    y))

;; Phase offset of triangle-converter to align with amplitude of sine
(defconstant +TRIANGLE-OFFSET+ (coerce (* 0.75 2.0 +SHORT-PI+) 'single-float))

(declaim (inline phase-triangle-converter))
(defun phase-triangle-converter (phi &key (phase-offset 0.0))
  "phi -- The phase in radiant. 0...POSITIVE-INFINITY
   phase-offset -- An optional phase offset in radians. Must be greater or equal zero"
  (declare (type single-float phi phase-offset))
  (let ((normalized (/ (mod (+ phi +TRIANGLE-OFFSET+ phase-offset) (* 2.0 +SHORT-PI+)) +SHORT-PI+))) ;; 0..2
    (let ((y (+ -1.0 (* 2.0 (abs (+ -1.0 normalized))))))
      y)))
