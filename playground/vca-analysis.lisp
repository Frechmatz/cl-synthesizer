(defpackage :cl-synthesizer-playground-vca-analysis
  (:use :cl))

(in-package :cl-synthesizer-playground-vca-analysis)

;; was soll mit diesem Beispiel analysiert werden:
;; Warum der exp output eine niedrigere Amplitude hat als der lineare output
;; TODO: Warum soll er die haben? Weil die Verstärker-Amplitude
;;   den gleichen Wert hat für Linear und Exponentiell
;;   Oki! Kann ich das mit Tests belegen?
(defun analysis()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))

    ;; Set up oszillator modulating the amplification
    (cl-synthesizer:add-module
     rack "LFO-CV"
     #'cl-synthesizer-modules-vco:vco
     :base-frequency 0.5
     :v-peak 2.5)
    ;; CV will be fed into two VCAs therefore we need a multiple
    (cl-synthesizer:add-module
     rack "LFO-CV-MULTIPLE"
     #'cl-synthesizer-modules-multiple:multiple
     :output-count 2)
    (cl-synthesizer:add-patch rack "LFO-CV" :triangle "LFO-CV-MULTIPLE" :input)

    ;; set up oszillator providing the audio signal
    (cl-synthesizer:add-module
     rack "VCO-AUDIO"
     #'cl-synthesizer-modules-vco:vco
     :base-frequency 10000.0
     :v-peak 2.5)
    ;; Audio signal will be fed into two VCAs therefore we need a multiple
    (cl-synthesizer:add-module
     rack "VCO-AUDIO-MULTIPLE"
     #'cl-synthesizer-modules-multiple:multiple
     :output-count 2)
    (cl-synthesizer:add-patch rack "VCO-AUDIO" :sine "VCO-AUDIO-MULTIPLE" :input)

    ;; Set up VCA which does not have a GAIN offset
    (cl-synthesizer:add-module
     rack "VCA-NO-GAIN"
     #'cl-synthesizer-modules-vca::vca-ng
     :cv-max 5.0)
    (cl-synthesizer:add-patch rack "VCO-AUDIO-MULTIPLE" :output-1 "VCA-NO-GAIN" :input)
    (cl-synthesizer:add-patch rack "LFO-CV-MULTIPLE" :output-1 "VCA-NO-GAIN" :cv)

    ;; Add monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "LFO-CV-MULTIPLE" :input-socket :input)
       (:channel-2 "VCO-AUDIO-MULTIPLE" :input-socket :input)
       (:channel-3 "VCA-NO-GAIN" :output-socket :output-linear)
       (:channel-4 "VCA-NO-GAIN" :output-socket :output-exponential)
       )
     :filename "/Users/olli/waves/vca-analyse.wav")

    rack))

(defun play-analysis()
  (cl-synthesizer-util:play-rack
   (cl-synthesizer-playground-vca-analysis::analysis)
   5))

;; (cl-synthesizer-playground-vca-analysis::play-analysis)





















