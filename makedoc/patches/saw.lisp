(defpackage :cl-synthesizer-patches-saw
  (:documentation "Saw")
  (:use :cl))
(in-package :cl-synthesizer-patches-saw)

(defun example ()
  (let ((rack (cl-synthesizer:make-rack
               :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 440.0 :v-peak 5.0)
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-file-agent:make-backend
     '(("VCO" :output-socket :saw))
     :filename "docs/saw.wav"
     :v-peak 5.0)
    rack))

(defun run-example ()
  (cl-synthesizer:play-rack (example) :duration-seconds 3.0))

;; (run-example)
