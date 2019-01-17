(defpackage :cl-synthesizer-profiling-waveform-converter
  (:use :cl))

(in-package :cl-synthesizer-profiling-waveform-converter)

;;
;; The tests add a bit overhead to prevent compiler from optimizations
;;

(defun run-sine (&key duration-seconds phi)
  (let ((result 0.0))
    (dotimes (i (* 44100 duration-seconds))
      (setf result (+ result (cl-synthesizer-core:phase-sine-converter phi :phase-offset 0.0)))
      (setf phi (+ 0.01 phi)))
    result))

(defun run-square (&key duration-seconds phi)
  (let ((result 0.0))
    (dotimes (i (* 44100 duration-seconds))
      (setf result (+ result (cl-synthesizer-core:phase-square-converter phi :phase-offset 0.0 :duty-cycle 0.5)))
      (setf phi (+ 0.01 phi)))
    result))

(defun run-triangle (&key duration-seconds phi)
  (let ((result 0.0))
    (dotimes (i (* 44100 duration-seconds))
      (setf result (+ result (cl-synthesizer-core:phase-triangle-converter phi :phase-offset 0.0)))
      (setf phi (+ 0.01 phi)))
    result))

(defun run-saw (&key duration-seconds phi)
  (let ((result 0.0))
    (dotimes (i (* 44100 duration-seconds))
      (setf result (+ result (cl-synthesizer-core:phase-saw-converter phi :phase-offset 0.0)))
      (setf phi (+ 0.01 phi)))
    result))
