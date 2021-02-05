(defpackage :cl-synthesizer-monitor-example-1
  (:use :cl))

(in-package :cl-synthesizer-monitor-example-1)

(defun example ()
  "Monitor example"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module
     rack
     "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 5.0 :v-peak 5.0)

    ;; Write the sine signal into a wave file
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-file-agent:make-backend
     '(("VCO" :output-socket :sine))
     :filename "cl-synthesizer-examples/monitor-example-1.wav"
     :v-peak 5.0)

    ;; Write all waveforms and the phase into a CSV file
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-file-agent:make-backend
     '(("VCO" :output-socket :sine :name "Sine")
       ("VCO" :output-socket :triangle :name "Triangle")
       ("VCO" :output-socket :saw :name "Saw")
       ("VCO" :output-socket :square :name "Square")
       ("VCO" :state :phase :name "Phase"))
     :filename "cl-synthesizer-examples/monitor-example-1.csv"
     :add-header t
     :column-separator ",")
    
    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer:play-rack rack :duration-seconds 2)))

;; (run-example)
