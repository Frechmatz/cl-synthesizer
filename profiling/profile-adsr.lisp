(defpackage :cl-synthesizer-profiling-adsr
  (:use :cl))

(in-package :cl-synthesizer-profiling-adsr)

(defun make-test-rack (&key exponential)
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))

    (cl-synthesizer:add-module
     rack "LFO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 20.0 ;; not too fast as we not want to abort any phase
     :v-peak 5.0)
  
    (cl-synthesizer:add-module
     rack "ADSR"
     #'cl-synthesizer-modules-adsr:make-module
     :attack-time-ms 5 :attack-target-output 5.0
     :decay-time-ms 10 :decay-target-output 4.0
     :release-time-ms 5
     :exponential exponential)
    
    (cl-synthesizer:add-patch rack "LFO" :square "ADSR" :gate)

    rack))
