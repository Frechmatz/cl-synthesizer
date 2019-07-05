(defpackage :cl-synthesizer-profiling-csv-file-writer
  (:use :cl))

(in-package :cl-synthesizer-profiling-csv-file-writer)

(defun make-test-rack (&key filename sample-rate)
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment :sample-rate sample-rate))))
    (cl-synthesizer:add-module rack "VCO"
			       #'cl-synthesizer-modules-vco:make-module
			       :base-frequency 5000.0
			       :v-peak 5.0)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("VCO" :output-socket :sine :name "Sine 1")
       ("VCO" :output-socket :sine :name "Sine 2")
       ("VCO" :output-socket :sine :name "Sine 3")
       ("VCO" :output-socket :sine :name "Sine 4"))
     ;;:filename "cl-synthesizer-examples/csv-profiling.csv"
     :filename filename
     :add-header t
     :column-separator ",")

    rack))

