(defpackage :cl-synthesizer-playground-profiling-phase-generator
  (:use :cl))

(in-package :cl-synthesizer-playground-profiling-phase-generator)

(defun run-phase-generator ()
  ""
  (let ((pg (cl-synthesizer-core:phase-generator 44100))
	(phi 0))
    (dotimes (i 10000000)
      (setf phi (funcall pg 440)))
    phi))


(defpackage :cl-synthesizer-playground-profiling-phase-generator-main
  (:use :cl))
(in-package :cl-synthesizer-playground-profiling-phase-generator-main)

(require :sb-sprof)
(defun profile ()
  (sb-profile:profile "CL-SYNTHESIZER-CORE" "CL-SYNTHESIZER-PLAYGROUND-PROFILING-PHASE-GENERATOR")
  (cl-synthesizer-playground-profiling-phase-generator::run-phase-generator)
  (sb-profile:report))

;;(profile)

(defun run-no-profiling ()
  (cl-synthesizer-playground-profiling-phase-generator::run-phase-generator))


;;(run-no-profiling)

