(defpackage :cl-synthesizer-modules-vco-example-3
  (:use :cl))

(in-package :cl-synthesizer-modules-vco-example-3)

(defparameter *attach-speaker* t)

(defun example ()
  "Play two sinus waves in stereo"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco-exponential :base-frequency 440 :f-max 8000 :v-peak 5)
    (cl-synthesizer:add-module rack "VCO-2"
				#'cl-synthesizer-modules-vco:vco-exponential :base-frequency 442 :f-max 8000 :v-peak 5)
    (cl-synthesizer:add-patch rack "VCO-1" :sine "LINE-OUT" :channel-1)
    (cl-synthesizer:add-patch rack "VCO-2" :sine "LINE-OUT" :channel-2)
    
    ;; Write LINE-OUT to Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "LINE-OUT" :input-socket :channel-1) (:channel-2 "LINE-OUT" :input-socket :channel-2))
     :filename "waves/vco-example-3.wav"
     :v-peak 5.0)

    rack))

;;(cl-synthesizer-util:play-rack (cl-synthesizer-modules-vco-example-3::example) 5 :attach-speaker *attach-speaker*)

