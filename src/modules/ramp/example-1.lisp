(defpackage :cl-synthesizer-modules-ramp-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-ramp-example-1)

(defun example ()
  "Ramp example"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module
     rack "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 0.5 :v-peak 5.0 :f-max 500 :cv-max 5)

    (cl-synthesizer:add-module
     rack "TRIGGER"
     #'cl-synthesizer-modules-cv-to-trigger:make-module
     :trigger-threshold 4.9 :pulse-voltage 5.0)

    (cl-synthesizer:add-module
     rack "ATTACK"
     #'cl-synthesizer-modules-ramp:make-module
     :time-ms 200 :target-output 5.0 :gate-state nil)

    (cl-synthesizer:add-module
     rack "DECAY"
     #'cl-synthesizer-modules-ramp:make-module
     :time-ms 200 :target-output 2.5)
    
    (cl-synthesizer:add-patch rack "VCO" :square "TRIGGER" :input)
    (cl-synthesizer:add-patch rack "TRIGGER" :output "ATTACK" :trigger)
    (cl-synthesizer:add-patch rack "ATTACK" :busy "DECAY" :pass-through)
    (cl-synthesizer:add-patch rack "ATTACK" :output "DECAY" :input)
    (cl-synthesizer:add-patch rack "ATTACK" :done "DECAY" :trigger)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("VCO" :output-socket :square :name "VCO Out" :format "~,5F")
       ("ATTACK" :output-socket :output :name "Attack Out" :format "~,5F")
       ("DECAY" :output-socket :output :name "Decay Out" :format "~,5F"))
     :filename "waves/ramp-example-1.csv")
    
    rack))

;;(cl-synthesizer:play-rack (example) 5)
