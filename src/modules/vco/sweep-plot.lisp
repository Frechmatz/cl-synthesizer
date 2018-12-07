(defpackage :cl-synthesizer-modules-vco-sweep-plot
  (:use :cl))

(in-package :cl-synthesizer-modules-vco-sweep-plot)

(defun example ()
  "Frequency sweep plots for analysis etc."
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))

    (cl-synthesizer:add-module
     rack
     "LFO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 0.25 :v-peak 5 :cv-max 5 :f-max 12000)

    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 1 :v-peak 5 :cv-max 5 :f-max 5000)

    (cl-synthesizer:add-patch rack "LFO" :triangle "VCO" :cv-lin)
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("VCO" :output-socket :triangle))
     :filename "waves/vco-sweep-plot-vco.wav")

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("LFO" :output-socket :triangle))
     :filename "waves/vco-sweep-plot-lfo.wav")

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("VCO" :state :frequency :format "~,4F"))
    :filename "waves/vco-sweep-plot-vco-frequency.csv")

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("VCO" :state :phi :format "~,10F"))
    :filename "waves/vco-sweep-plot-vco-phi.csv")
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("VCO" :output-socket :triangle :format "~,4F"))
    :filename "waves/vco-sweep-plot-vco.csv")
    
    rack))
      
;;(cl-synthesizer:play-rack (example) 4)

