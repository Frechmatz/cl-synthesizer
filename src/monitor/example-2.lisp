(defpackage :cl-synthesizer-monitor-example-2
  (:use :cl))

(in-package :cl-synthesizer-monitor-example-2)

(defun example ()
  "CSV-Handler example"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module
     rack "VCO" #'cl-synthesizer-modules-vco:make-module :base-frequency 5.0 :v-peak 5.0)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-backend
     '(("VCO" :output-socket :sine :name "Sine"))
     :filename "cl-synthesizer-examples/monitor-example-2.csv"
     :add-header t
     :column-separator ",")
    
    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer:play-rack rack :duration-seconds 2)))

;; (run-example)
