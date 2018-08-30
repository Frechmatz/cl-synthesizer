(defpackage :cl-synthesizer-modules-vco-example-2
  (:use :cl))

(in-package :cl-synthesizer-modules-vco-example-2)

(defun example ()
  "A Triangle Sweep"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack "LFO-1"
     #'cl-synthesizer-modules-vco::vco-linear :v-peak 5 :cv-max 5 :base-frequency 2 :f-max 12000)

    (cl-synthesizer:add-module rack "VCO-1"
			       #'cl-synthesizer-modules-vco::vco-linear :base-frequency 50 :cv-max 5 :v-peak 5 :f-max 12000)
    (cl-synthesizer:add-patch rack "LFO-1" :triangle "VCO-1" :cv)
    
    ;; Write Sweep to Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '(
       (:channel-1 "LFO-1" :output-socket :triangle) 
       (:channel-2 "VCO-1" :input-socket :cv)
       (:channel-3 "VCO-1" :output-socket :triangle)
       )
     :filename "/Users/olli/waves/vco-example-2.wav")

    rack))

;;(cl-synthesizer-util:play-rack (cl-synthesizer-modules-vco-example-2::example) 5)
  
