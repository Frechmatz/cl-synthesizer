(defpackage :cl-synthesizer-monitor-example-1
  (:use :cl))

(in-package :cl-synthesizer-monitor-example-1)

(defun example ()
  "Write all wave forms into a Wave and into a CSV file"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-linear-vco:make-module
     :base-frequency 10 :v-peak 5 :cv-max 5 :f-max 12000)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '(("VCO" :output-socket :sine)
       ("VCO" :output-socket :square)
       ("VCO" :output-socket :triangle)
       ("VCO" :output-socket :saw))
     :filename "waves/monitor-example-1.wav")

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:csv-file-handler
     '(("VCO" :output-socket :sine :format "~,4F" :name "Sine")
       ("VCO" :output-socket :square :format "~,4F" :name "Square")
       ("VCO" :output-socket :triangle :format "~,4F" :name "Triangle")
       ("VCO" :output-socket :saw :format "~,4F" :name "Saw"))
     :filename "waves/monitor-example-1.csv"
     :add-header t
     :column-separator ",")
    
    rack))
      
;;(cl-synthesizer:play-rack (example) 1)

