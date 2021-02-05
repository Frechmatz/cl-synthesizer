(defpackage :cl-synthesizer-modules-vco-example-2
  (:use :cl))

(in-package :cl-synthesizer-modules-vco-example-2)

(defun example ()
  "A frequency modulated triangle"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack "LFO"
     #'cl-synthesizer-modules-vco:make-module :v-peak 5.0 :base-frequency 4.0)

    (cl-synthesizer:add-module
     rack "VCO"
     #'cl-synthesizer-modules-vco:make-module :base-frequency 440.0 :v-peak 5.0 :cv-lin-hz-v 20.0)

    (cl-synthesizer:add-patch rack "LFO" :triangle "VCO" :cv-lin)
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-file-agent:make-backend
     '(("VCO" :output-socket :triangle))
     :filename "cl-synthesizer-examples/vco-example-2.wav"
     :v-peak 5.0)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-file-agent:make-backend
     '(("VCO" :state :frequency :name "Frequency"))
    :filename "cl-synthesizer-examples/vco-example-2.csv")
    
    rack))

(defun run-example ()
  (let ((rack (example))) (cl-synthesizer:play-rack rack :duration-seconds 2)))

;; (run-example)

  
