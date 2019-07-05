(defpackage :cl-synthesizer-profiling-vco
  (:use :cl))

(in-package :cl-synthesizer-profiling-vco)

(defun make-test-rack (&key vco-count)
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    (dotimes (i vco-count)
      (cl-synthesizer:add-module rack (format nil "MODULE-~a" i)
				 #'cl-synthesizer-modules-vco:make-module
				 :base-frequency 5000.0
				 :v-peak 5.0))
    rack))

