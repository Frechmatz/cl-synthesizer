(defpackage :cl-synthesizer-modules-vco-example-3
  (:use :cl))

(in-package :cl-synthesizer-modules-vco-example-3)

(defparameter *attach-audio* t)

(defun example ()
  "Play two sinus waves in stereo"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :output-sockets '(:line-out-1 :line-out-2))))
    (cl-synthesizer:add-module
     rack "VCO-1"
     #'cl-synthesizer-modules-exponential-vco:make-module :base-frequency 440 :f-max 8000 :v-peak 5)
    (cl-synthesizer:add-module
     rack "VCO-2"
     #'cl-synthesizer-modules-exponential-vco:make-module :base-frequency 442 :f-max 8000 :v-peak 5)
    (cl-synthesizer:add-patch rack "VCO-1" :sine "OUTPUT" :line-out-1)
    (cl-synthesizer:add-patch rack "VCO-2" :sine "OUTPUT" :line-out-2)
    
    ;; Write LINE-OUT to Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "OUTPUT" :input-socket :line-out-1)
       (:channel-2 "OUTPUT" :input-socket :line-out-2))
     :filename "waves/vco-example-3.wav"
     :v-peak 5.0)

    rack))

#|
(cl-synthesizer::play-rack (example) 5
    :attach-audio t :audio-output-sockets '(:line-out-1 :line-out-2))
|#
