(defpackage :cl-synthesizer-experimental-example-modem
  (:use :cl))

(in-package :cl-synthesizer-experimental-example-modem)

(defun example ()
  "Emulate the sound of a modem data transfer"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       ;; Expose line-out sockets
	       :output-sockets '(:audio))))

    ;; LFO controls the number of "data transfers"
    (cl-synthesizer:add-module
     rack "MAIN-LFO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 0.1 :v-peak 5.0
     :phase-offset (coerce (* 1.5 PI) 'single-float))

    (cl-synthesizer:add-module
     rack
     "GATE"
     #'cl-synthesizer-modules-multiple:make-module :output-count 2)

    (cl-synthesizer:add-patch rack "MAIN-LFO" :square "GATE" :input)
    
    ;; Envelope controls frequency and duration of negotiation and transfer phases
    (cl-synthesizer:add-module
     rack "ADSR"
     #'cl-synthesizer-modules-adsr:make-module
     :gate-threshold 4.9
     ;; Negotiation phase
     :attack-time-ms 3000 :attack-target-output 0.05
     ;; Data transfer phase
     :decay-time-ms 50 :decay-target-output 0.30
     :release-time-ms 10)

    (cl-synthesizer:add-patch rack "GATE" :output-1 "ADSR" :gate)
    
    (cl-synthesizer:add-module
     rack "LFO-2"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 0.2 :v-peak 0.2 :cv-lin-hz-v 1.0)

    (cl-synthesizer:add-module
     rack "VCO-1"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 440.0 :v-peak 5.0 :cv-lin-hz-v 1.0)
    
    ;; (cl-synthesizer:add-patch rack "LFO-1" :saw "LFO-2" :cv-lin)
    (cl-synthesizer:add-patch rack "ADSR" :cv "LFO-2" :cv-lin)
    (cl-synthesizer:add-patch rack "LFO-2" :square "VCO-1" :cv-lin)

    ;; VCA to shutdown the signal when Gate goes down
    (cl-synthesizer:add-module
     rack "VCA"
     #'cl-synthesizer-modules-vca:make-module
     :cv-max 5.0)
    
    (cl-synthesizer:add-patch rack "VCO-1" :square "VCA" :input)
    (cl-synthesizer:add-patch rack "GATE" :output-2 "VCA" :cv)
    (cl-synthesizer:add-patch rack "VCA" :output-linear "OUTPUT" :audio)
    
    ;; Debugging...
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-backend
     '(("ADSR" :input-socket :gate :name "ADSR Gate")
       ("ADSR" :output-socket :cv :name "ADSR Out")
       ("VCA" :input-socket :cv :name "VCA CV")       
       ("VCO-1" :state :frequency :name "Frequency")
       ("OUTPUT" :input-socket :audio :name "Audio"))
     :filename "cl-synthesizer-examples/experimental-example-modem-adsr.csv"
     :add-header t
     :column-separator ",")

    rack))

(defun run-example ()
  (cl-synthesizer-experimental:play-rack
   (example)
   :duration-seconds 15
   :attach-audio t
   :audio-output-sockets '(:audio)))
  
;;(run-example)

