(defpackage :cl-synthesizer-profiling-phase-generator
  (:use :cl))

(in-package :cl-synthesizer-profiling-phase-generator)

(defun run-phase-generator (&key duration-seconds)
  (let ((pg (cl-synthesizer-util:phase-generator 44100.0))
	(phi 0))
    (dotimes (i (* 44100 duration-seconds))
      (setf phi (funcall pg 440.0)))
    phi))

