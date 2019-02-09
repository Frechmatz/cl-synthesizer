(defpackage :cl-synthesizer-modules-vco-example-4
  (:use :cl))

(in-package :cl-synthesizer-modules-vco-example-4)

(defun example ()
  "Plots phase and frequency of a frequency sweep."
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))

    (cl-synthesizer:add-module
     rack
     "LFO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 0.25 :v-peak 5.0 :cv-max 5.0 :f-max 12000.0)

    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 1.0 :v-peak 5.0 :cv-max 5.0 :f-max 5000.0)

    (cl-synthesizer:add-patch rack "LFO" :triangle "VCO" :cv-lin)
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("LFO" :output-socket :triangle :name "LFO-Triangle"  :format "~,4F")
       ("VCO" :state :frequency :name "VCO-Frequency" :format "~,4F")
       ("VCO" :state :phi :name "VCO-Phase" :format "~,10F")
       ("VCO" :output-socket :triangle :name "VCO-Triangle"  :format "~,4F"))
    :filename "cl-synthesizer-examples/vco-example-4.csv")

    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer::play-rack
     rack
     4)))

;; (run-example)

