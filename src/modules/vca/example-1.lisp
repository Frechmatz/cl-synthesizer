(defpackage :cl-synthesizer-modules-vca-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-vca-example-1)

(defun example ()
  "Amplification of a 10kHz sine wave with a bipolar triangular signal."
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))

    ;; Set up oscillator modulating the amplification
    (cl-synthesizer:add-module
     rack "LFO-CV"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 0.5
     :v-peak 5.0)

    ;; set up oscillator providing the audio signal
    (cl-synthesizer:add-module
     rack "VCO-AUDIO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 5.0
     :v-peak 5.0)

    ;; Set up VCA
    (cl-synthesizer:add-module
     rack "VCA"
     #'cl-synthesizer-modules-vca:make-module
     :cv-max 5.0
     :exponential t)

    ;; Add patches
    (cl-synthesizer:add-patch rack "VCO-AUDIO" :sine "VCA" :input)
    (cl-synthesizer:add-patch rack "LFO-CV" :triangle "VCA" :cv)

    ;; Write VCA inputs/outputs to a CSV file
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-file-agent:make-backend
     '(("VCA" :input-socket :cv :name "CV")
       ("VCA" :input-socket :input :name "Input")
       ("VCA" :output-socket :output :name "Output"))
     :filename "cl-synthesizer-examples/vca-example-1.csv")
    
    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer:play-rack rack :duration-seconds 5.0)))

;; (run-example)

