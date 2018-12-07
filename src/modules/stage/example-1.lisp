(defpackage :cl-synthesizer-modules-stage-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-stage-example-1)

(defun example ()
  "Ramp example"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module
     rack "LFO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 0.5 :v-peak 5.0 :f-max 500 :cv-max 5)

    (cl-synthesizer:add-module
     rack "TRIGGER"
     #'cl-synthesizer-modules-cv-to-trigger:make-module
     :trigger-cv 4.9 :pulse-voltage 5.0)

    ;; Attack
    (cl-synthesizer:add-module
     rack "RAMP-1"
     #'cl-synthesizer-modules-ramp-stage:make-module
     :time-ms 200 :target-output 5.0 :gate-state nil)

    ;; Decay
    (cl-synthesizer:add-module
     rack "RAMP-2"
     #'cl-synthesizer-modules-ramp-stage:make-module
     :time-ms 200 :target-output 2.5)
    
    (cl-synthesizer:add-patch rack "LFO" :square "TRIGGER" :input)
    (cl-synthesizer:add-patch rack "TRIGGER" :output "RAMP-1" :trigger)

    (cl-synthesizer:add-patch rack "RAMP-1" :busy "RAMP-2" :pass-through)
    (cl-synthesizer:add-patch rack "RAMP-1" :output "RAMP-2" :input)
    (cl-synthesizer:add-patch rack "RAMP-1" :done "RAMP-2" :trigger)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("LFO" :output-socket :square :name "LFO Out" :format "~,5F")
       ("RAMP-1" :output-socket :output :name "Ramp 1 Out" :format "~,5F")
       ("RAMP-2" :output-socket :output :name "Ramp 2 Out" :format "~,5F"))
     :filename "waves/stage-example-1.csv")
    
    
    rack))

;;(cl-synthesizer:play-rack (example) 5)
