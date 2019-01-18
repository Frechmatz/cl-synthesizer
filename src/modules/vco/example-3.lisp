(defpackage :cl-synthesizer-modules-vco-example-3
  (:use :cl))

(in-package :cl-synthesizer-modules-vco-example-3)

(defparameter *attach-audio* nil)

(defun example ()
  "Play two sinus waves in stereo"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :output-sockets '(:line-out-1 :line-out-2))))
    (cl-synthesizer:add-module
     rack "VCO-1"
     #'cl-synthesizer-modules-vco:make-module :base-frequency 440.0 :f-max 8000.0 :v-peak 5.0 :cv-max 5.0)
    (cl-synthesizer:add-module
     rack "VCO-2"
     #'cl-synthesizer-modules-vco:make-module :base-frequency 442.0 :f-max 8000.0 :v-peak 5.0 :cv-max 5.0)
    (cl-synthesizer:add-patch rack "VCO-1" :sine "OUTPUT" :line-out-1)
    (cl-synthesizer:add-patch rack "VCO-2" :sine "OUTPUT" :line-out-2)
    
    ;; Write LINE-OUT to Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("OUTPUT" :input-socket :line-out-1)
       ("OUTPUT" :input-socket :line-out-2))
     :filename "cl-synthesizer-examples/vco-example-3.wav"
     :v-peak 5.0)

    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer::play-rack
     rack
     5
     :attach-audio *attach-audio* :audio-output-sockets '(:line-out-1 :line-out-2))))

;; (run-example)
