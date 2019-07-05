(defpackage :cl-synthesizer-rack-example-sine
  (:use :cl))

(in-package :cl-synthesizer-rack-example-sine)

(defun example ()
  "Generate a sine wave file"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 440.0 :v-peak 5.0)

    ;; Generate a Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("VCO" :output-socket :sine))
     :filename "cl-synthesizer-examples/rack-example-sine.wav")

    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer:play-rack rack :duration-seconds 5)))

;; (run-example)
