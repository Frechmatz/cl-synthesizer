(defpackage :cl-synthesizer-modules-system-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-system-example-1)

(defun example ()
  "System example"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module
     rack "SYSTEM"
     #'cl-synthesizer-modules-system:make-module)
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-file-agent:make-backend
     '(("SYSTEM" :output-socket :ticks :name "ticks")
       ("SYSTEM" :output-socket :milliseconds :name "milliseconds")
       ("SYSTEM" :output-socket :seconds :name "seconds")
       ("SYSTEM" :output-socket :sample-rate :name "sample-rate"))
     :filename "cl-synthesizer-examples/system-example-1.csv"
     :add-header t
     :column-separator ",")

    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer:play-rack rack :duration-seconds 2)))

;; (run-example)
