(defpackage :cl-synthesizer-modules-vco-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-vco-example-1)

(defun example ()
  "Write all wave forms into a 4-Channel wave file"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 10 :v-peak 5 :cv-max 5 :f-max 12000)
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("VCO" :output-socket :sine)
       ("VCO" :output-socket :triangle)
       ("VCO" :output-socket :saw)
       ("VCO" :output-socket :square))
     :filename "waves/vco-example-1.wav")

    rack))
      
;;(cl-synthesizer:play-rack (example) 3)

