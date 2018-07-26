;;
;;
;; Exponential VCA Playground
;;
;;

(defpackage :cl-synthesizer-playground-exp-vca
  (:use :cl))

(in-package :cl-synthesizer-playground-exp-vca)

(defun synthesizer-playground-exponential-vca ()
  "Exponential VCA example"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module
     rack "LFO"
     #'cl-synthesizer-modules-vco:vco
     :base-frequency 1.0
     :v-peak 5)
    
    (cl-synthesizer:add-module
     rack "VCA"
     #'cl-synthesizer-modules-vca::vca-ng
     :max-amplification 5.0
     :max-amplification-cv 5.0)
    
    ;; LFO modulates amplification of VCA
    (cl-synthesizer:add-patch rack "LFO" :triangle "VCA" :cv)
    ;; Feed a constant voltage into the VCA :input socket
    (cl-synthesizer:add-module rack "CONST-VOLTAGE" #'cl-synthesizer-modules-fixed-output:fixed-output :value 1.0)
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
