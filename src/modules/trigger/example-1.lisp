(defpackage :cl-synthesizer-modules-trigger-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-trigger-example-1)

(defun example ()
  "Emit trigger signal based on sine input"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 5.0 :v-peak 5.0)

    (cl-synthesizer:add-module
     rack
     "TRIGGER"
     #'cl-synthesizer-modules-trigger:make-module
     :trigger-threshold 4.9 :pulse-voltage 3.0)

    (cl-synthesizer:add-patch rack "VCO" :sine "TRIGGER" :input)
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-backend
     '(("TRIGGER" :input-socket :input)
       ("TRIGGER" :output-socket :output))
     :filename "cl-synthesizer-examples/trigger-example-1.wav")

    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer:play-rack rack :duration-seconds 2)))

;; (run-example)
