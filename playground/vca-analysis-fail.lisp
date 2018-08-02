(defpackage :cl-synthesizer-playground-vca-fail
  (:use :cl))

(in-package :cl-synthesizer-playground-vca-fail)


(defun vca-fail ()
  "Exponential VCA example"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))

    ;; Set up oszillator modulating the amplification
    (cl-synthesizer:add-module
     rack "LFO-CV"
     #'cl-synthesizer-modules-vco:vco
     :base-frequency 0.5
     :v-peak 2.5)

    ;; set up oszillator providing the audio signal
    (cl-synthesizer:add-module
     rack "VCO-AUDIO"
     #'cl-synthesizer-modules-vco:vco
     :base-frequency 5000.0
     :v-peak 2.5)

    (cl-synthesizer:add-module
     rack "VCA"
     #'cl-synthesizer-modules-vca::vca-ng
     :cv-max 2.5
     :cv-initial-gain 2.5)
    (cl-synthesizer:add-patch rack "VCO-AUDIO" :sine "VCA" :input)
    (cl-synthesizer:add-patch rack "LFO-CV" :triangle "VCA" :cv)

    ;; Add monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "VCA" :input-socket :input)
       (:channel-2 "VCA" :input-socket :cv)
       (:channel-3 "VCA" :output-socket :output-linear)
       (:channel-4 "VCA" :output-socket :output-exponential))
     :filename "/Users/olli/waves/vcaplayground-fail.wav")

    rack))

(defun play ()
  (cl-synthesizer-util:play-rack
   (vca-fail)
   5))

;; (play)


