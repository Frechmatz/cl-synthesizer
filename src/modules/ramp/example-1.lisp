(defpackage :cl-synthesizer-modules-ramp-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-ramp-example-1)

(defun make-voice (name environment &key (exponential nil))
  (declare (ignore name))
  (let ((rack
         (cl-synthesizer:make-rack
          :environment environment
	  :input-sockets '(:trigger)
	  :output-sockets '(:output))))

    (cl-synthesizer:add-module
     rack "ATTACK"
     #'cl-synthesizer-modules-ramp:make-module
     :time-ms 700 :target-output 5.0 :gate-state nil :exponential exponential)

    (cl-synthesizer:add-module
     rack "DECAY"
     #'cl-synthesizer-modules-ramp:make-module
     :time-ms 500 :target-output -2.5 :exponential exponential)
    
    (cl-synthesizer:add-patch rack "INPUT" :trigger "ATTACK" :trigger)
    (cl-synthesizer:add-patch rack "ATTACK" :busy "DECAY" :pass-through)
    (cl-synthesizer:add-patch rack "ATTACK" :output "DECAY" :input)
    (cl-synthesizer:add-patch rack "ATTACK" :done "DECAY" :trigger)

    (cl-synthesizer:add-patch rack "DECAY" :output "OUTPUT" :output)

    rack))

(defun example ()
  "Linear and Exponential Ramp"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))

    (cl-synthesizer:add-module
     rack "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 0.5 :v-peak 5.0)

    (cl-synthesizer:add-module
     rack "TRIGGER"
     #'cl-synthesizer-modules-trigger:make-module
     :trigger-threshold 4.9 :pulse-voltage 5.0)

    (cl-synthesizer:add-patch rack "VCO" :square "TRIGGER" :input)
    
    (cl-synthesizer:add-module
     rack "TRIGGER-MULTIPLE"
     #'cl-synthesizer-modules-multiple:make-module
     :output-count 2)

    (cl-synthesizer:add-patch rack "TRIGGER" :output "TRIGGER-MULTIPLE" :input)
    
    (cl-synthesizer:add-module
     rack "LINEAR" #'make-voice :exponential nil)
    (cl-synthesizer:add-patch rack "TRIGGER-MULTIPLE" :output-1 "LINEAR" :trigger)
    
    (cl-synthesizer:add-module
     rack "EXPONENTIAL" #'make-voice :exponential t)
    (cl-synthesizer:add-patch rack "TRIGGER-MULTIPLE" :output-2 "EXPONENTIAL" :trigger)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-backend
     '(("LINEAR" :output-socket :output :name "Lin Out")
       ("EXPONENTIAL" :output-socket :output :name "Exp Out"))
     :filename "cl-synthesizer-examples/ramp-example-1.csv")
    
    rack))

(defun run-example ()
  (let ((rack (example))) (cl-synthesizer:play-rack rack :duration-seconds 5)))

;; (run-example)
