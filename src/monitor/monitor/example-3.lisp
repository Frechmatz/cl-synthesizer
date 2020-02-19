(defpackage :cl-synthesizer-monitor-example-3
  (:use :cl))

(in-package :cl-synthesizer-monitor-example-3)

(defun example ()
  "Wave-Handler example"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module
     rack "VCO" #'cl-synthesizer-modules-vco:make-module
     :base-frequency 5.0 :v-peak 5.0)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-backend
     '(("VCO" :output-socket :sine))
     :filename "cl-synthesizer-examples/monitor-example-3.wav")
    
    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer:play-rack rack :duration-seconds 2)))

;; (run-example)
