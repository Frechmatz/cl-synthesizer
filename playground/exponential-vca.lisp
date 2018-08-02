;;
;;
;; Exponential VCA Playground
;;
;;

(defpackage :cl-synthesizer-playground-exp-vca
  (:use :cl))

(in-package :cl-synthesizer-playground-exp-vca)

#|
(defun synthesizer-playground-exponential-vca ()
  "Exponential VCA example"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment)))
	(fixed-vca-input 1.0))
    
    (cl-synthesizer:add-module
     rack "LFO"
     #'cl-synthesizer-modules-vco:vco
     :base-frequency 1.0
     :v-peak 5)
    
    (cl-synthesizer:add-module
     rack "VCA"
     #'cl-synthesizer-modules-vca::vca-ng
     :input-max fixed-vca-input
     :output-max 5.0
     :cv-max 5.0)
    
    ;; LFO modulates amplification of VCA
    (cl-synthesizer:add-patch rack "LFO" :triangle "VCA" :cv)
    ;; Feed a constant voltage into the VCA :input socket
    (cl-synthesizer:add-module rack "CONST-VOLTAGE" #'cl-synthesizer-modules-fixed-output:fixed-output :value fixed-vca-input)
    (cl-synthesizer:add-patch rack "CONST-VOLTAGE" :out "VCA" :input)

    ;; Add monitor
    ;; expected output:
    ;; both outputs of VCA should reach the maximum volatge of 5.0
    ;; but with different curve progressions
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "VCA" :input-socket :cv)
       (:channel-2 "VCA" :input-socket :input)
       (:channel-3 "VCA" :output-socket :output-linear)
       (:channel-4 "VCA" :output-socket :output-exponential)
       )
     :filename "/Users/olli/waves/vcaplayground.wav")

    rack))

(defun play ()
  (cl-synthesizer-util:play-rack
   (synthesizer-playground-exponential-vca)
   5))

;; (play)



|#

#|

(defun synthesizer-playground-exponential-vca-gain ()
  "Exponential VCA example"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment)))
	(fixed-vca-input 1.0))
    
    (cl-synthesizer:add-module
     rack "LFO"
     #'cl-synthesizer-modules-vco:vco
     :base-frequency 1.0
     :v-peak 5)
    
    (cl-synthesizer:add-module
     rack "VCA"
     #'cl-synthesizer-modules-vca::vca-ng
     :input-max fixed-vca-input
     :output-max 5.0
     :cv-max 10.0
     :cv-initial-gain 8.0)
    
    ;; LFO modulates amplification of VCA
    (cl-synthesizer:add-patch rack "LFO" :triangle "VCA" :cv)
    ;; Feed a constant voltage into the VCA :input socket
    (cl-synthesizer:add-module rack "CONST-VOLTAGE" #'cl-synthesizer-modules-fixed-output:fixed-output :value fixed-vca-input)
    (cl-synthesizer:add-patch rack "CONST-VOLTAGE" :out "VCA" :input)

    ;; Add monitor
    ;; expected output:
    ;; both outputs of VCA should reach the maximum volatge of 5.0
    ;; but with different curve progressions
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "VCA" :input-socket :cv)
       (:channel-2 "VCA" :input-socket :input)
       (:channel-3 "VCA" :output-socket :output-linear)
       (:channel-4 "VCA" :output-socket :output-exponential)
       )
     :filename "/Users/olli/waves/vcaplaygroundgain.wav")

    rack))

(defun play-2 ()
  (cl-synthesizer-util:play-rack
   (synthesizer-playground-exponential-vca-gain)
   5))

;; (play-2)

|#






#|








(defun synthesizer-playground-exponential-vca-gain-and-no-gain ()
  "Exponential VCA example"
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
     :base-frequency 5000.0
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

    ;; Set up VCA which has a GAIN offset
    (cl-synthesizer:add-module
     rack "VCA-GAIN"
     #'cl-synthesizer-modules-vca::vca-ng
     :cv-max 5.0
     :cv-initial-gain 2.5)
    (cl-synthesizer:add-patch rack "VCO-AUDIO-MULTIPLE" :output-2 "VCA-GAIN" :input)
    (cl-synthesizer:add-patch rack "LFO-CV-MULTIPLE" :output-2 "VCA-GAIN" :cv)

    ;; Add monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "LFO-CV-MULTIPLE" :input-socket :input)
       (:channel-2 "VCO-AUDIO-MULTIPLE" :input-socket :input)
       (:channel-3 "VCA-NO-GAIN" :output-socket :output-linear)
       (:channel-4 "VCA-NO-GAIN" :output-socket :output-exponential)
       (:channel-5 "VCA-GAIN" :output-socket :output-linear)
       (:channel-6 "VCA-GAIN" :output-socket :output-exponential))
     :filename "/Users/olli/waves/vcaplayground-gain-and-no-gain.wav")

    rack))

(defun play-synthesizer-playground-exponential-vca-gain-and-no-gain ()
  (cl-synthesizer-util:play-rack
   (synthesizer-playground-exponential-vca-gain-and-no-gain)
   5))

;; (play-synthesizer-playground-exponential-vca-gain-and-no-gain)



|#





#|

(defun synthesizer-playground-exponential-vca-no-gain-1 ()
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

    ;; Set up VCA which does not have a GAIN offset
    (cl-synthesizer:add-module
     rack "VCA"
     #'cl-synthesizer-modules-vca::vca-ng
     :cv-max 2.5)
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
     :filename "/Users/olli/waves/vcaplayground-no-gain-1.wav")

    rack))

(defun play-synthesizer-playground-exponential-vca-no-gain-1 ()
  (cl-synthesizer-util:play-rack
   (synthesizer-playground-exponential-vca-no-gain-1)
   5))

;; (play-synthesizer-playground-exponential-vca-no-gain-1)





|#





(defun synthesizer-playground-exponential-vca-gain2-5 ()
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

    ;; Set up VCA which does not have a GAIN offset
    (cl-synthesizer:add-module
     rack "VCA"
     #'cl-synthesizer-modules-vca::vca-ng
     :cv-max 2.5
     :cv-initial-gain 2.5
     )
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
     :filename "/Users/olli/waves/vcaplayground-gain2-5.wav")

    rack))

(defun play-synthesizer-playground-exponential-vca-gain2-5 ()
  (cl-synthesizer-util:play-rack
   (synthesizer-playground-exponential-vca-gain2-5)
   5))

;; (play-synthesizer-playground-exponential-vca-gain2-5)







