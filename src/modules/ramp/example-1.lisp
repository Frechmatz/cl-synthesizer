(defpackage :cl-synthesizer-modules-ramp-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-ramp-example-1)

(defun example ()
  "Ramp example"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module
     rack "LFO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 0.25 :v-peak 5.0 :f-max 500 :cv-max 5)

    (cl-synthesizer:add-module
     rack "TRIGGER"
     #'cl-synthesizer-modules-cv-to-trigger:make-module
     :trigger-cv 4.9 :pulse-voltage 5.0)

    (cl-synthesizer:add-module
     rack "RAMP"
     #'cl-synthesizer-modules-ramp:make-module
     :time-ms 500 :target-value 3.0)
    
    (cl-synthesizer:add-patch rack "LFO" :square "TRIGGER" :input)
    (cl-synthesizer:add-patch rack "TRIGGER" :output "RAMP" :trigger)
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("LFO" :output-socket :square)
       ("TRIGGER" :output-socket :output)
       ("RAMP" :output-socket :output)
       ("RAMP" :output-socket :busy)
       ("RAMP" :output-socket :done))
     :filename "waves/ramp-example-1.wav")
    
    
    rack))

;;(cl-synthesizer:play-rack (example) 5)
