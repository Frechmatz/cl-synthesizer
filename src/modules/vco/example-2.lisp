(defpackage :cl-synthesizer-modules-vco-example-2
  (:use :cl))

(in-package :cl-synthesizer-modules-vco-example-2)

(defun example ()
  "A frequency modulated triangle"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack "LFO"
     ;; 0.08 => +/- 80Hz (80 * (5/5000))
     ;; VCO Frequency range = 360..520Hz
     #'cl-synthesizer-modules-vco:make-module :v-peak 0.08 :cv-max 5.0 :base-frequency 4.0 :f-max 20.0)

    (cl-synthesizer:add-module
     rack "VCO"
     #'cl-synthesizer-modules-vco:make-module :base-frequency 440.0 :cv-max 5.0 :v-peak 5.0 :f-max 5000.0)

    (cl-synthesizer:add-patch rack "LFO" :triangle "VCO" :cv-lin)
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("VCO" :output-socket :triangle))
     :filename "cl-synthesizer-examples/vco-example-2.wav")

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("VCO" :state :frequency :name "Frequency" :format "~,4F"))
    :filename "cl-synthesizer-examples/vco-example-2.csv")
    
    rack))

(defun run-example ()
  (let ((rack (example))) (cl-synthesizer:play-rack rack :duration-seconds 2)))

;; (run-example)

  
