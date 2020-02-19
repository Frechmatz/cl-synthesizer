(defpackage :cl-synthesizer-patches-sine
  (:documentation "440Hz sine")
  (:use :cl))

(in-package :cl-synthesizer-patches-sine)

(defun example ()
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :output-sockets '(:sine))))
    
    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 440.0 :v-peak 5.0)

    (cl-synthesizer:add-patch rack "VCO" :sine "OUTPUT" :sine)
    
    ;; We do not want to have a hard wired Wave-Writer module in our rack.
    ;; Let the file writing stuff be handled by a monitor.
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-backend
     '(("VCO" :output-socket :sine))
     :filename "src/lisp/cl-synthesizer/docs/sine.wav")

    rack))

(defparameter *duration-seconds* 3.0)
;; parameter and corresponding OUTPUT patches only required for experimental audio output
(defparameter *audio-output-sockets* '(:sine))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer:play-rack rack :duration-seconds *duration-seconds*)))

;; (run-example)
