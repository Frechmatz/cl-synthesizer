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
     #'cl-synthesizer-modules-vco:make-module :v-peak 0.08 :cv-max 5 :base-frequency 4 :f-max 20)

    (cl-synthesizer:add-module
     rack "VCO"
     #'cl-synthesizer-modules-vco:make-module :base-frequency 440 :cv-max 5 :v-peak 5 :f-max 5000)

    (cl-synthesizer:add-patch rack "LFO" :triangle "VCO" :cv-lin)
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("VCO" :output-socket :triangle))
     :filename "waves/vco-example-2.wav")

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("VCO" :state :frequency :name "Frequency" :format "~,4F"))
    :filename "waves/vco-example-2.csv")
    
    rack))

;;(cl-synthesizer:play-rack (example) 2)
  
