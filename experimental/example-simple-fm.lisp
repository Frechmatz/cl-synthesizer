(defpackage :cl-synthesizer-experimental-example-simple-fm
  (:use :cl))

(in-package :cl-synthesizer-experimental-example-simple-fm)

(defun example ()
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       ;; Expose line-out sockets
	       :output-sockets '(:audio))))

    (cl-synthesizer:add-module
     rack "VCO-1"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 300.0 :v-peak 5.0)

    (cl-synthesizer:add-module
     rack "VCO-2"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 440.0 :v-peak 5.0 :cv-lin-hz-v 100.0)

    (cl-synthesizer:add-patch rack "VCO-1" :sine "VCO-2" :cv-lin)
    (cl-synthesizer:add-patch rack "VCO-2" :sine "OUTPUT" :audio)

    rack))

(defun run-example ()
  (cl-synthesizer-experimental:play-rack
   (example)
   :duration-seconds 5
   :attach-audio t
   :audio-output-sockets '(:audio)))
  
;;(run-example)

