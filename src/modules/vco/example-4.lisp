(defpackage :cl-synthesizer-modules-vco-example-4
  (:use :cl))

(in-package :cl-synthesizer-modules-vco-example-4)

(defparameter *attach-speaker* nil)

(defun example ()
  "Modulate the frequency of a saw signal with a LFO."
  (let ((rack (cl-synthesizer:make-rack
	       :environment
	       (cl-synthesizer:make-environment
		:output-directory "/Users/olli/waves/"))))

    (cl-synthesizer:add-module
     rack "LFO-1"
     #'cl-synthesizer-modules-vco:vco-linear
     :base-frequency 1.0 :v-peak 1.0 :f-max 500 :cv-max 5)

    (cl-synthesizer:add-module
     rack "LFO-2"
     #'cl-synthesizer-modules-vco:vco-linear
     :base-frequency 2.0 :v-peak 1.0 :f-max 500 :cv-max 5)

    (cl-synthesizer:add-module
     rack "VCO-1"
     #'cl-synthesizer-modules-vco:vco-linear
     :base-frequency 440 :f-max 5000 :v-peak 5 :cv-max 5)

    (cl-synthesizer:add-module
     rack "VCO-2"
     #'cl-synthesizer-modules-vco:vco-linear
     :base-frequency 442 :f-max 5000 :v-peak 5 :cv-max 5)
    
    (cl-synthesizer:add-patch rack "LFO-1" :sine "VCO-1" :cv)
    (cl-synthesizer:add-patch rack "LFO-2" :sine "VCO-2" :cv)
    (cl-synthesizer:add-patch rack "VCO-1" :saw "LINE-OUT" :channel-1)
    (cl-synthesizer:add-patch rack "VCO-2" :saw "LINE-OUT" :channel-2)

    ;; Write LFO/VCO outputs to Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "LFO-1" :output-socket :sine)
       (:channel-2 "LFO-2" :output-socket :sine)
       (:channel-3 "VCO-1" :output-socket :saw)
       (:channel-4 "VCO-2" :output-socket :saw))
     :filename "vco-example-4-recorder.wav")
    
    ;; Write LINE-OUT to Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "LINE-OUT" :input-socket :channel-1)
       (:channel-2 "LINE-OUT" :input-socket :channel-2))
     :filename "vco-example-4.wav")
    
    rack))

;; Execute to run rack
;;(cl-synthesizer:play-rack (example) 5 :attach-speaker *attach-speaker*)


