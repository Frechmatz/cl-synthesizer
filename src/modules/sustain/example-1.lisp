(defpackage :cl-synthesizer-modules-sustain-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-sustain-example-1)

(defun example ()
  "Ramp example"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    
    ;; Add sequencer
    (cl-synthesizer:add-module
     rack
     "MIDI-SEQUENCER"
     #'cl-synthesizer-modules-midi-sequencer:make-module :events
     (list 
      (list :timestamp-milli-seconds 50
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
      (list :timestamp-milli-seconds 700
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))))

    ;; Add MIDI Interface and connect it with the MIDI Sequencer
    (cl-synthesizer:add-module
     rack
     "MIDI-IFC"
     #'cl-synthesizer-modules-midi-interface:make-module :voice-count 1)
    (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-IFC" :midi-events)

    (cl-synthesizer:add-module
     rack
     "GATE-MULTIPLE"
     #'cl-synthesizer-modules-multiple:make-module :output-count 2)

    (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "GATE-MULTIPLE" :input)
    
    (cl-synthesizer:add-module
     rack "TRIGGER"
     #'cl-synthesizer-modules-cv-to-trigger:make-module
     :trigger-cv 4.9 :pulse-voltage 5.0)

    ;; Attack
    (cl-synthesizer:add-module
     rack "ATTACK"
     #'cl-synthesizer-modules-ramp:make-module
     :time-ms 300 :target-output 5.0 :gate-state :on)

    ;; Sustain
    (cl-synthesizer:add-module
     rack "SUSTAIN"
     #'cl-synthesizer-modules-sustain:make-module)
    
    ;; Release
    (cl-synthesizer:add-module
     rack "RELEASE"
     #'cl-synthesizer-modules-ramp:make-module
     :time-ms 700 :target-output 0.0)
    
    (cl-synthesizer:add-patch rack "GATE-MULTIPLE" :output-1 "TRIGGER" :input)
    (cl-synthesizer:add-patch rack "GATE-MULTIPLE" :output-2 "ATTACK" :gate)
    
    (cl-synthesizer:add-patch rack "TRIGGER" :output "ATTACK" :trigger)

    ;; ATTACK => SUSTAIN
    (cl-synthesizer:add-patch rack "ATTACK" :busy "SUSTAIN" :pass-through)
    (cl-synthesizer:add-patch rack "ATTACK" :output "SUSTAIN" :input)
    (cl-synthesizer:add-patch rack "ATTACK" :gate "SUSTAIN" :gate)
    (cl-synthesizer:add-patch rack "ATTACK" :done "SUSTAIN" :trigger)

    ;; SUSTAIN => RELEASE
    (cl-synthesizer:add-patch rack "SUSTAIN" :busy "RELEASE" :pass-through)
    (cl-synthesizer:add-patch rack "SUSTAIN" :output "RELEASE" :input)
    (cl-synthesizer:add-patch rack "SUSTAIN" :gate "RELEASE" :gate)
    (cl-synthesizer:add-patch rack "SUSTAIN" :done "RELEASE" :trigger)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(
       ("MIDI-IFC" :output-socket :gate-1 :name "MIDI Gate" :format "~,5F")
       ("TRIGGER" :output-socket :output :name "Trigger Out" :format "~,5F")
       
       ("ATTACK" :output-socket :done :name "Attack Done" :format "~,5F")
       ("ATTACK" :output-socket :output :name "Attack Out" :format "~,5F")
       ("RELEASE" :output-socket :output :name "Release Out" :format "~,5F"))
     :filename "waves/sustain-example-1.csv")
     
    
    rack))

;;(cl-synthesizer:play-rack (example) 3)
