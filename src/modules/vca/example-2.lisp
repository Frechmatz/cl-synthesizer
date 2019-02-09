(defpackage :cl-synthesizer-modules-vca-example-2
  (:use :cl))

(in-package :cl-synthesizer-modules-vca-example-2)


(defun example ()
  "Amplification of a 10kHz sine wave with a bipolar triangular signal. A 
   gain of 2.5 will be added to the amplification signal."
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))

    ;; Set up oszillator modulating the amplification
    (cl-synthesizer:add-module
     rack "LFO-CV"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 0.5
     :cv-max 5.0
     :f-max 12000.0
     :v-peak 2.5) ;; will be moved up to 5.0 via gain of VCA

    ;; set up oszillator providing the audio signal
    (cl-synthesizer:add-module
     rack "VCO-AUDIO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 10000.0
     :cv-max 5.0
     :f-max 12000.0
     :v-peak 5.0)

    ;; Set up VCA with Gain
    (cl-synthesizer:add-module
     rack "VCA"
     #'cl-synthesizer-modules-vca:make-module
     :cv-max 5.0
     :initial-gain 2.5)

    ;; Add patches
    (cl-synthesizer:add-patch rack "VCO-AUDIO" :sine "VCA" :input)
    (cl-synthesizer:add-patch rack "LFO-CV" :triangle "VCA" :cv)

    ;; Record VCA inputs/outputs into a Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("VCA" :input-socket :cv)
       ("VCA" :input-socket :input)
       ("VCA" :output-socket :output-linear)
       ("VCA" :output-socket :output-exponential))
     :filename "cl-synthesizer-examples/vca-example-2.wav")
    
    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer:play-rack rack :duration-seconds 120)))

;; (run-example)



