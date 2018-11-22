(defpackage :cl-synthesizer-modules-vco-example-2
  (:use :cl))

(in-package :cl-synthesizer-modules-vco-example-2)

(defun example ()
  "A frequency modulated triangle"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack "LFO-1"
     ;; 0.08 => +/- 80Hz (80 * (5/5000))
     #'cl-synthesizer-modules-vco-ng:make-module :v-peak 0.08 :cv-max 5 :base-frequency 4 :f-max 20)

    (cl-synthesizer:add-module
     rack "VCO-1"
     #'cl-synthesizer-modules-vco-ng:make-module :base-frequency 440 :cv-max 5 :v-peak 5 :f-max 5000)

    (cl-synthesizer:add-patch rack "LFO-1" :triangle "VCO-1" :cv-lin)
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("VCO-1" :output-socket :triangle))
     :filename "waves/vco-example-2.wav")

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("VCO-1" :state :frequency :name "Frequency" :format "~,4F"))
    :filename "waves/vco-example-2.csv"
    :add-header nil)
    
    rack))

;;(cl-synthesizer:play-rack (example) 2)
  
