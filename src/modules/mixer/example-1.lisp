(defpackage :cl-synthesizer-modules-mixer-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-mixer-example-1)

(defun example ()
  "Mixer example."
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :output-sockets '(:line-out))))

    ;;
    ;; add modules...
    ;;
    
    (cl-synthesizer:add-module
     rack "MIXER" #'cl-synthesizer-modules-mixer:mixer
     :channel-count 2
     :channel-cv-max 5.0
     :channel-cv-gain 5.0
     :main-cv-max 5.0
     :main-cv-gain 2.5)
    
    (cl-synthesizer:add-patch rack "VOICE-1" :audio "MIXER" :channel-1)
    (cl-synthesizer:add-patch rack "VOICE-2" :audio "MIXER" :channel-1)
    (cl-synthesizer:add-patch rack "MIXER" :output "OUTPUT" :line-out)
    
    rack))



