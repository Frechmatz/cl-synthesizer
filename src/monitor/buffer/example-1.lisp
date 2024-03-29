(defpackage :cl-synthesizer-monitor-buffer-example-1
  (:use :cl))

(in-package :cl-synthesizer-monitor-buffer-example-1)

(defun example ()
  "Buffer-Agent example"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module
     rack "VCO" #'cl-synthesizer-modules-vco:make-module :base-frequency 5.0 :v-peak 5.0)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-buffer-agent:make-backend
     '(("VCO" :output-socket :sine)
       ("VCO" :output-socket :saw))
     :buffer (make-array 2))
    
    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer:play-rack rack :duration-seconds 2)))

;; (run-example)
