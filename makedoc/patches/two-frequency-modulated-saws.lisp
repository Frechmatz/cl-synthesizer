(defpackage :cl-synthesizer-patches-two-frequency-modulated-saws
  (:use :cl)
  (:documentation "Two frequency modulated Saws"))
(in-package :cl-synthesizer-patches-two-frequency-modulated-saws)

(defun make-modulated-saw (name environment &key lfo-frequency vco-frequency)
  (declare (ignore name))
  (let ((rack (cl-synthesizer:make-rack :environment environment)))
    (cl-synthesizer:add-module
     rack
     "LFO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency lfo-frequency :v-peak 5.0)
    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency vco-frequency :v-peak 5.0 :cv-lin-hz-v 20.0)
    (cl-synthesizer:add-patch rack "LFO" :sine "VCO" :cv-lin)
    (cl-synthesizer:add-rack-output rack :saw "VCO" :saw)
    rack))

(defun example ()
  (let ((rack (cl-synthesizer:make-rack
               :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack "SAW-1" #'make-modulated-saw :lfo-frequency 1.0 :vco-frequency 440.0)
    (cl-synthesizer:add-module
     rack "SAW-2" #'make-modulated-saw :lfo-frequency 2.0 :vco-frequency 442.0)
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-file-agent:make-backend
     '(("SAW-1" :output-socket :saw)
       ("SAW-2" :output-socket :saw))
     :filename "docs/two-frequency-modulated-saws.wav"
     :v-peak 5.0)
    rack))

(defun run-example ()
  (cl-synthesizer:play-rack (example) :duration-seconds 3.0))

;;(run-example)
