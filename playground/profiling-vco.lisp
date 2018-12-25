(defpackage :cl-synthesizer-playground-profiling-vco
  (:use :cl))

(in-package :cl-synthesizer-playground-profiling-vco)

(defun make-module (name environment)
  (declare (ignore name))
  (let ((rack (cl-synthesizer:make-rack :environment environment)))
    
    (cl-synthesizer:add-module
     rack "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 5000
     :f-max 13000
     :cv-max 5.0
     :v-peak 5)
    
    rack))

(defun make-test-rack ()
  "Test rack"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    (dotimes (i 10)
      (cl-synthesizer:add-module rack (format nil "MODULE-~a" i) #'make-module))
    rack))

(require :sb-sprof)
(defun run-rack ()
  (sb-profile:profile "CL-SYNTHESIZER" "CL-SYNTHESIZER-MODULES-VCO" "CL-SYNTHESIZER-CORE")
  (cl-synthesizer:play-rack (make-test-rack) 10)
  (sb-profile:report))

;;(run-rack)

(defun run-rack-no-profiling ()
  (cl-synthesizer:play-rack (make-test-rack) 10))

;;(run-rack-no-profiling)

