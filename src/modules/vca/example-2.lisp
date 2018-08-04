;;
;; How to run this example:
;; (asdf:load-system :cl-synthesizer :force t)
;; Load this file (slime-load-file)
;; Execute function cl-synthesizer-modules-vca-example-2::play-example
;;

(defpackage :cl-synthesizer-modules-vca-example-2
  (:use :cl))

(in-package :cl-synthesizer-modules-vca-example-2)


(defun example ()
  "Amplification of a 10kHz sine wave with a bipolar triangular amplification signal. A 
   gain of 2.5 will be added to the amplification  signal.
  The input and output signals of the VCA are written into a 4-Channel Wave-File"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))

    ;; Set up oszillator modulating the amplification
    (cl-synthesizer:add-module
     rack "LFO-CV"
     #'cl-synthesizer-modules-vco:vco
     :base-frequency 0.5
     :v-peak 2.5) ;; will be moved up to 5.0 via gain of VCA

    ;; set up oszillator providing the audio signal
    (cl-synthesizer:add-module
     rack "VCO-AUDIO"
     #'cl-synthesizer-modules-vco:vco
     :base-frequency 10000.0
     :v-peak 5.0)

    ;; Set up VCA with Gain
    (cl-synthesizer:add-module
     rack "VCA"
     #'cl-synthesizer-modules-vca:vca
     :cv-max 5.0
     :cv-initial-gain 2.5)

    ;; Add patches
    (cl-synthesizer:add-patch rack "VCO-AUDIO" :sine "VCA" :input)
    (cl-synthesizer:add-patch rack "LFO-CV" :triangle "VCA" :cv)

    ;; Record VCA inputs/outputs into a Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "VCA" :input-socket :cv)
       (:channel-2 "VCA" :input-socket :input)
       (:channel-3 "VCA" :output-socket :output-linear)
       (:channel-4 "VCA" :output-socket :output-exponential))
     :filename "/Users/olli/waves/vca-example-2.wav")

    rack))

(defun play-example()
  (cl-synthesizer-util:play-rack
   (example)
   5))

;; (cl-synthesizer-modules-vca-example-2::play-example)
