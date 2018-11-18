(defpackage :cl-synthesizer-modules-cv-to-trigger-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-cv-to-trigger-example-1)

(defun example ()
  "Emit trigger signal based on sine input"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-linear-vco:make-module
     :base-frequency 5 :v-peak 5 :cv-max 5 :f-max 12000)

    (cl-synthesizer:add-module
     rack
     "TRIGGER"
     #'cl-synthesizer-modules-cv-to-trigger:make-module
     :trigger-cv 2.5 :pulse-voltage 3.0)

    (cl-synthesizer:add-patch rack "VCO" :sine "TRIGGER" :input)
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '(("TRIGGER" :input-socket :input)
       ("TRIGGER" :output-socket :output))
     :filename "waves/cv-to-trigger-example-1.wav")

    rack))
      
;;(cl-synthesizer:play-rack (example) 2)

