(defpackage :cl-synthesizer-patches-frequency-modulated-saw
  (:use :cl)
  (:documentation "Frequency modulated Saw"))
(in-package :cl-synthesizer-patches-frequency-modulated-saw)

(defun example ()
  (let ((rack (cl-synthesizer:make-rack
               :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack
     "LFO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 1.0
     :v-peak 5.0)
    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 440
     :v-peak 5.0
     :cv-lin-hz-v 20.0)
    (cl-synthesizer:add-patch rack "LFO" :sine "VCO" :cv-lin)
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-file-agent:make-backend
     '(("VCO" :output-socket :saw))
     :filename "docs/frequency-modulated-saw.wav"
     :v-peak 5.0)
    rack))

(defun run-example ()
  (cl-synthesizer:play-rack (example) :duration-seconds 3.0))

;;(run-example)
