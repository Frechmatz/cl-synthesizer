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
     :base-frequency -5000 :v-peak 5 :cv-max 5 :f-max 12000)
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("VCO" :state :phi :name "Phase" :format "~,10F"))
     :filename "phase-plot.csv"
     :add-header nil)

    rack))
      
;;(cl-synthesizer:play-rack (example) 3)

