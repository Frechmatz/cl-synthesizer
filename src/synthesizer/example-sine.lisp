(defpackage :cl-synthesizer-rack-example-sine
  (:documentation "Example: Generate a 440Hz sine wave and write it to a wave file.")
  (:use :cl))

(in-package :cl-synthesizer-rack-example-sine)

(defun example ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 440.0 :v-peak 5.0)

    ;; We do not want to have a hard wired Wave-Writer module in our rack.
    ;; Let the file writing stuff be handled by a monitor.
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-backend
     '(("VCO" :output-socket :sine))
     :filename "cl-synthesizer-examples/rack-example-sine.wav"
     :v-peak 5.0)

    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer:play-rack rack :duration-seconds 5)))

;; (run-example)
