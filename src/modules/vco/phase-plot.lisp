(defpackage :cl-synthesizer-modules-vco-phase-plot
  (:use :cl))

(in-package :cl-synthesizer-modules-vco-phase-plot)

(defun example ()
  "Create a phase plot"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency -5000.0 :v-peak 5.0 :cv-max 5.0 :f-max 12000.0)
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("VCO" :state :phi :name "Phase" :format "~,10F"))
     :filename "phase-plot.csv")

    rack))
      
;;(cl-synthesizer:play-rack (example) 3)

