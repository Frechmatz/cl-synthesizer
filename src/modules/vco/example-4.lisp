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
     :base-frequency 0.25 :v-peak 5.0)

    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 1.0 :v-peak 5.0 :cv-lin-hz-v 100.0)

    (cl-synthesizer:add-patch rack "LFO" :triangle "VCO" :cv-lin)
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-file-agent:make-backend
     '(("LFO" :output-socket :triangle :name "LFO-Triangle")
       ("VCO" :state :frequency :name "VCO-Frequency")
       ("VCO" :state :phase :name "VCO-Phase")
       ("VCO" :output-socket :triangle :name "VCO-Triangle"))
    :filename "cl-synthesizer-examples/vco-example-4.csv")

    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer::play-rack rack :duration-seconds 4)))

;; (run-example)

