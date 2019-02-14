(defpackage :cl-synthesizer-profiling-wave-file-writer
  (:use :cl))

(in-package :cl-synthesizer-profiling-wave-file-writer)

(defun make-test-rack (&key sample-rate filename)
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment :sample-rate sample-rate))))
    (cl-synthesizer:add-module rack "VCO"
			       #'cl-synthesizer-modules-vco:make-module
			       :base-frequency 5000.0
			       :f-max 13000.0
			       :cv-max 5.0
			       :v-peak 5.0)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("VCO" :output-socket :sine :name "Sine 1")
       ("VCO" :output-socket :sine :name "Sine 2")
       ("VCO" :output-socket :sine :name "Sine 3")
       ("VCO" :output-socket :sine :name "Sine 4"))
     :filename filename)

    rack))
