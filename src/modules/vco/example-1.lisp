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
     :base-frequency 10.0 :v-peak 5.0 :cv-max 5.0 :f-max 12000.0)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("VCO" :output-socket :sine)
       ("VCO" :output-socket :triangle)
       ("VCO" :output-socket :saw)
       ("VCO" :output-socket :square))
     :filename "waves/vco-example-1.wav")
    
    rack))

#|
(let ((rack (example)))
  (time (cl-synthesizer:play-rack rack 60)))
|#


