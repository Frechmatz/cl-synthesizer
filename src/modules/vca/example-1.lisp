(defpackage :cl-synthesizer-modules-vca-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-vca-example-1)

(defun example ()
  "Amplification of a 10kHz sine wave with a bipolar triangular signal."
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))

    ;; Set up oscillator modulating the amplification
    (cl-synthesizer:add-module
     rack "LFO-CV"
     #'cl-synthesizer-modules-linear-vco:make-module
     :base-frequency 0.5
     :v-peak 5.0
     :cv-max 5.0
     :f-max 12000)

    ;; set up oscillator providing the audio signal
    (cl-synthesizer:add-module
     rack "VCO-AUDIO"
     #'cl-synthesizer-modules-linear-vco:make-module
     :base-frequency 10000.0
     :v-peak 5.0
     :cv-max 5.0
     :f-max 12000)

    ;; Set up VCA
    (cl-synthesizer:add-module
     rack "VCA"
     #'cl-synthesizer-modules-vca:make-module
     :cv-max 5.0)

    ;; Add patches
    (cl-synthesizer:add-patch rack "VCO-AUDIO" :sine "VCA" :input)
    (cl-synthesizer:add-patch rack "LFO-CV" :triangle "VCA" :cv)

    ;; Record VCA inputs/outputs into a Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '(("VCA" :input-socket :cv)
       ("VCA" :input-socket :input)
       ("VCA" :output-socket :output-linear)
       ("VCA" :output-socket :output-exponential))
     :filename "waves/vca-example-1.wav")

    rack))

;;(cl-synthesizer:play-rack (example) 5)
