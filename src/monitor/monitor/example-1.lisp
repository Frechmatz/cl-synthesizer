(defpackage :cl-synthesizer-monitor-example-1
  (:use :cl))

(in-package :cl-synthesizer-monitor-example-1)

(defun example ()
  "Write all wave forms into a Wave and into a CSV file"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 10.0 :v-peak 5.0 :cv-max 5.0 :f-max 12000.0)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("VCO" :output-socket :sine)
       ("VCO" :output-socket :square)
       ("VCO" :output-socket :triangle)
       ("VCO" :output-socket :saw))
     :filename "cl-synthesizer-examples/monitor-example-1.wav")

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("VCO" :output-socket :sine :format "~,4F" :name "Sine")
       ("VCO" :output-socket :square :format "~,4F" :name "Square")
       ("VCO" :output-socket :triangle :format "~,4F" :name "Triangle")
       ("VCO" :output-socket :saw :format "~,4F" :name "Saw"))
     :filename "cl-synthesizer-examples/monitor-example-1.csv"
     :add-header t
     :column-separator ",")
    
    rack))

(defun run-example ()
  (cl-synthesizer:play-rack (example) :duration-seconds 1))

;; (run-example)
