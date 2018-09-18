(defpackage :cl-synthesizer-modules-vco-example-4
  (:use :cl))

(in-package :cl-synthesizer-modules-vco-example-4)

(defparameter *attach-speaker* t)

(defun example ()
  "Modulate sine with sine-lfo"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "LFO-1"
				#'cl-synthesizer-modules-vco:vco-linear :base-frequency 1.0 :v-peak 1.0 :f-max 500 :cv-max 5)
    (cl-synthesizer:add-module rack "LFO-2"
				#'cl-synthesizer-modules-vco:vco-linear :base-frequency 1.0 :v-peak 1.0 :f-max 500 :cv-max 5)
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco-linear :base-frequency 440 :f-max 5000 :v-peak 5 :cv-max 5)
    (cl-synthesizer:add-module rack "VCO-2"
				#'cl-synthesizer-modules-vco:vco-linear :base-frequency 440 :f-max 5000 :v-peak 5 :cv-max 5)
    
    (cl-synthesizer:add-patch rack "LFO-1" :sine "VCO-1" :cv)
    (cl-synthesizer:add-patch rack "LFO-2" :sine "VCO-2" :cv)
    (cl-synthesizer:add-patch rack "VCO-1" :sine "LINE-OUT" :channel-1)
    (cl-synthesizer:add-patch rack "VCO-2" :sine "LINE-OUT" :channel-2)

    ;; Write LFO/VCO outputs to Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "LFO-1" :output-socket :sine)
       (:channel-2 "LFO-2" :output-socket :sine)
       (:channel-3 "VCO-1" :output-socket :sine)
       (:channel-4 "VCO-2" :output-socket :sine))
     :filename "waves/vco-example-4-oscilloscope.wav")
    
    ;; Write LINE-OUT to Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "LINE-OUT" :input-socket :channel-1) (:channel-2 "LINE-OUT" :input-socket :channel-2))
     :filename "waves/vco-example-4.wav")
    
    rack))
      
;;(cl-synthesizer:play-rack (cl-synthesizer-modules-vco-example-4::example) 5 :attach-speaker *attach-speaker*)


