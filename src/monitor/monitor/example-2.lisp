(defpackage :cl-synthesizer-monitor-example-2
  (:use :cl))

(in-package :cl-synthesizer-monitor-example-2)

(defun example ()
  "Write all wave forms into a CSV file"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-linear-vco:make-module
     :base-frequency 10 :v-peak 5 :cv-max 5 :f-max 12000)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "VCO" :output-socket :sine)
       (:channel-2 "VCO" :output-socket :square)
       (:channel-3 "VCO" :output-socket :triangle)
       (:channel-4 "VCO" :output-socket :saw))
     :filename "waves/monitor-example-2.wav")

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:csv-file-handler
     '((:column-1 "VCO" :output-socket :sine :format "~,4F" :name "Sine")
       (:column-2 "VCO" :output-socket :square :format "~,4F" :name "Square")
       (:column-3 "VCO" :output-socket :triangle :format "~,4F" :name "Triangle")
       (:column-4 "VCO" :output-socket :saw :format "~,4F" :name "Saw"))
     :filename "waves/monitor-example-2.csv"
     :add-header t
     :column-separator ",")
    
    rack))
      
;;(cl-synthesizer:play-rack (example) 1)

