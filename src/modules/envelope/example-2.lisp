(defpackage :cl-synthesizer-modules-envelope-example-2
  (:use :cl))

(in-package :cl-synthesizer-modules-envelope-example-2)

(defparameter *attach-speaker* t)

(defun example ()
  "Modulate Envelope Attack Time with LFO"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))

    (cl-synthesizer:add-module
     rack
     "MIDI-SEQUENCER"
     #'cl-synthesizer-modules-midi-sequencer:midi-sequencer :events
     (list 
      (list :timestamp-milli-seconds 0
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
      (list :timestamp-milli-seconds 1000
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))

      (list :timestamp-milli-seconds 2000
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
      (list :timestamp-milli-seconds 3000
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))

      (list :timestamp-milli-seconds 4000
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
      (list :timestamp-milli-seconds 5000
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))

      (list :timestamp-milli-seconds 6000
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
      (list :timestamp-milli-seconds 7000
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))

      (list :timestamp-milli-seconds 8000
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
      (list :timestamp-milli-seconds 9000
	    :midi-events (list
			  (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))))
    
    ;; Set up MIDI Interface and connect it with the MIDI Sequencer
    (cl-synthesizer:add-module
     rack
     "MIDI-IFC"
     #'cl-synthesizer-modules-midi-interface:midi-interface :voice-count 1)
    (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-IFC" :midi-events)

    ;; Add LFO
    (cl-synthesizer:add-module
     rack "LFO"
     #'cl-synthesizer-modules-vco:vco-linear :base-frequency 0.05 :v-peak 5 :cv-max 5.0 :f-max 12000)
    
    ;; Envelope
    (cl-synthesizer:add-module
     rack "ADSR"
     #'cl-synthesizer-modules-envelope:envelope
     :segments
     '(;; Attack
       (:duration-ms 100 :target-cv 5 :required-gate-state :on
	:duration-controller
	(:socket :attack-duration :input-min 0.0 :input-max 5.0 :output-min 0 :output-max 800))
       ;; Decay
       (:duration-ms 50 :target-cv 3 :required-gate-state :on)
       ;; Sustain
       (:required-gate-state :on)
       ;; Release
       (:duration-ms 100 :target-cv 0 :required-gate-state :off)))
    (cl-synthesizer:add-patch rack "LFO" :saw "ADSR" :attack-duration)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "ADSR" :gate)
    
    ;; Add VCO, VCA
    (cl-synthesizer:add-module
     rack "VCO" #'cl-synthesizer-modules-vco:vco-exponential
     :base-frequency (cl-synthesizer-midi:get-note-number-frequency 0)
     :f-max 12000
     :v-peak 5)
    (cl-synthesizer:add-module rack "VCA" #'cl-synthesizer-modules-vca:vca
			       :cv-max 5.0)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :cv-1 "VCO" :cv)
    (cl-synthesizer:add-patch rack "VCO" :sine "VCA" :input)
    (cl-synthesizer:add-patch rack "VCA" :output-linear "LINE-OUT" :channel-1)
    (cl-synthesizer:add-patch rack "ADSR" :cv "VCA" :cv)

    ;; Record ADSR-Output, LFO-Output and LINE-OUT into a wave file
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "ADSR" :output-socket :cv)
       (:channel-2 "LFO" :output-socket :saw)
       (:channel-3 "LINE-OUT" :input-socket :channel-1))
     :filename "waves/envelope-example-1.wav")
    
    rack))

;;(cl-synthesizer:play-rack (example) 10 :attach-speaker *attach-speaker*)
