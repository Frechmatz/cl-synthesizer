(defpackage :cl-synthesizer-modules-vco-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-vco-example-1)

(defun example ()
  "Write all wave forms into a Wave and a Csv file"
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
       ("VCO" :output-socket :triangle)
       ("VCO" :output-socket :saw)
       ("VCO" :output-socket :square))
     :filename "cl-synthesizer-examples/vco-example-1.wav")

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("VCO" :output-socket :sine :name "Sine")
       ("VCO" :output-socket :triangle :name "Triangle")
       ("VCO" :output-socket :saw :name "Saw")
       ("VCO" :output-socket :square :name "Square"))
     :filename "cl-synthesizer-examples/vco-example-1.csv"
     :add-header t
     :column-separator ",")
    
    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer:play-rack rack :duration-seconds 1)))

;; (run-example)

