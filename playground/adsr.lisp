;;
;;
;; ADSR Playground
;;
;;

(defpackage :cl-synthesizer-playground-adsr
  (:use :cl))

(in-package :cl-synthesizer-playground-adsr)

(defparameter *adsr-environment* (cl-synthesizer::make-environment))

(defun synthesizer-playground-adsr ()
  "ADSR-Example"
  (let ((rack (cl-synthesizer:create-rack :environment *adsr-environment*)))

    ;; Set up MIDI Interface and connect it to the MIDI input of the Rack
    (cl-synthesizer::add-module rack "MIDI-IFC"
				#'cl-synthesizer-modules-midi-interface:midi-interface :voice-count 1)
    (cl-synthesizer::add-patch rack "MIDI-IN" :midi-events "MIDI-IFC" :midi-events)

    ;; Add VCO, ADSR, VCA
    (cl-synthesizer::add-module rack "VCO" #'cl-synthesizer-modules-vco:vco
				:base-frequency (cl-synthesizer-midi:get-note-number-frequency 0)
				:cv-max 5
				:f-max 13000
				:v-peak 5)
    (cl-synthesizer::add-module rack "ADSR" #'cl-synthesizer-modules-adsr:adsr
				:attack-ms 1000
				:attack-cv 5
				:decay-ms 1000
				:decay-cv 3
				:release-ms 1000)
    (cl-synthesizer::add-module rack "VCA" #'cl-synthesizer-modules-vca:vca)

    ;; Attach modules to the outputs of the MIDI Interface
    (cl-synthesizer::add-patch rack "MIDI-IFC" :cv-1 "VCO" :cv)
    (cl-synthesizer::add-patch rack "MIDI-IFC" :gate-1 "ADSR" :gate)

    ;; Set up the Wave-File-Recorder
    ;; Writes the ADSR-Output and the VCA-Output into a 2 channel wave file
    (cl-synthesizer::add-module
     rack "WAVE-WRITER"
     #'cl-synthesizer-modules-wave-file-writer::two-channel-wave-file-writer
     :filename "/Users/olli/waves/adsrplayground.wav")
    
    ;; Attach output of VCO to VCA
    (cl-synthesizer::add-patch rack "VCO" :sine "VCA" :input)

    ;; Attach output of VCA to LINE-OUT and Recorder
    (cl-synthesizer::add-module rack "MULTIPLE-VCA-OUT" #'cl-synthesizer-modules-multiple:multiple-4)
    (cl-synthesizer::add-patch rack "VCA" :out "MULTIPLE-VCA-OUT" :input)
    (cl-synthesizer::add-patch rack "MULTIPLE-VCA-OUT" :out-1 "LINE-OUT" :channel-1)
    (cl-synthesizer::add-patch rack "MULTIPLE-VCA-OUT" :out-2 "WAVE-WRITER" :channel-1)

    ;; Attach output of ADSR to VCA and Recorder
    (cl-synthesizer::add-module rack "MULTIPLE-ADSR-OUT" #'cl-synthesizer-modules-multiple:multiple-4)
    (cl-synthesizer::add-patch rack "ADSR" :cv "MULTIPLE-ADSR-OUT" :input)
    (cl-synthesizer::add-patch rack "MULTIPLE-ADSR-OUT" :out-1 "VCA" :cv)
    (cl-synthesizer::add-patch rack "MULTIPLE-ADSR-OUT" :out-2 "WAVE-WRITER" :channel-2)

    ;; Add VCA output monitor
    (cl-synthesizer:register-monitor
     rack
     "My Monitor"
     #'cl-synthesizer-monitor:wave-file-handler
     '((:channel-1 "MULTIPLE-VCA-OUT" :output-socket :out-1))
     :filename "/Users/olli/waves/monitor.wav")
  
    rack))

(defun play ()
  (cl-synthesizer-util:play-rack
   (synthesizer-playground-adsr)
   10 
   :attach-speaker t
   :midi-device
   (cl-synthesizer-device-midi-sequencer:midi-sequencer
    "Midi-Device"
    *adsr-environment*
    :events
    (list 
     (list :timestamp-milli-seconds 20
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
     (list :timestamp-milli-seconds 2020
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))

     (list :timestamp-milli-seconds 3020
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
     (list :timestamp-milli-seconds 3520
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))
     ))))

;; (play)

(defun play2 ()
  (cl-synthesizer-util:play-rack
   (synthesizer-playground-adsr)
   7 
   :attach-speaker t
   :midi-device
   (cl-synthesizer-device-midi-sequencer:midi-sequencer
    "Midi-Device"
    *adsr-environment*
    :events
    (list 
     (list :timestamp-milli-seconds 20
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
     (list :timestamp-milli-seconds 4020
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))

     ))))

;; (play2)


(defun play3 ()
  (cl-synthesizer-util:play-rack
   (synthesizer-playground-adsr)
   10 
   :attach-speaker t
   :midi-device
   (cl-synthesizer-device-midi-sequencer:midi-sequencer
    "Midi-Device"
    *adsr-environment*
    :events
    (list 
     (list :timestamp-milli-seconds 20
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
     (list :timestamp-milli-seconds 700
	   :midi-events (list
			 (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))

     ))))

;; (play3)


