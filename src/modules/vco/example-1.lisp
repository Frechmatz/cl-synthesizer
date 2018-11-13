(defpackage :cl-synthesizer-modules-vco-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-vco-example-1)

(defun example ()
  "Write all wave forms into a 4-Channel wave file"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-linear-vco:make-module
     :base-frequency 10 :v-peak 5 :cv-max 5 :f-max 12000)
    
    ;; Record outputs into a Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "VCO" :output-socket :sine)
       (:channel-2 "VCO" :output-socket :triangle)
       (:channel-3 "VCO" :output-socket :saw)
       (:channel-4 "VCO" :output-socket :square))
     :filename "waves/vco-example-1.wav")

    rack))
      
;;(cl-synthesizer:play-rack (example) 3)

