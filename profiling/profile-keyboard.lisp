(defpackage :cl-synthesizer-profiling-keyboard
  (:use :cl))

(in-package :cl-synthesizer-profiling-keyboard)

(defun make-voice (name environment)
  (declare (ignore name))
  (let ((rack (cl-synthesizer:make-rack
	       :environment environment
	       :input-sockets '(:cv :gate)
	       :output-sockets '(:audio))))
    
    (cl-synthesizer:add-module
     rack "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency (cl-synthesizer-midi:get-note-number-frequency 0)
     :v-peak 5.0)

    (cl-synthesizer:add-module
     rack "ADSR"
     #'cl-synthesizer-modules-adsr:make-module
     :attack-time-ms 50 :attack-target-output 5.0
     :decay-time-ms 20 :decay-target-output 3.0
     :release-time-ms 30
     :backward-coupled t)
    
    (cl-synthesizer:add-module rack "VCA" #'cl-synthesizer-modules-vca:make-module :cv-max 5.0)

    (cl-synthesizer:add-patch rack "VCO" :sine "VCA" :input)
    (cl-synthesizer:add-patch rack "ADSR" :cv "VCA" :cv)
    (cl-synthesizer:add-patch rack "INPUT" :cv "VCO" :cv-exp)
    (cl-synthesizer:add-patch rack "INPUT" :gate "ADSR" :gate)
    (cl-synthesizer:add-patch rack "VCA" :output-linear "OUTPUT" :audio)
    
    rack))


(defun make-midi-event-segment (milli-seconds)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (list
   (list :timestamp-milli-seconds milli-seconds
	 :midi-events (list
		       (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
   (list :timestamp-milli-seconds (+ milli-seconds 100)
	 :midi-events (list
		       (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))
   (list :timestamp-milli-seconds (+ milli-seconds 200)
	 :midi-events (list
		       (cl-synthesizer-midi-event:make-note-on-event 1 75 100)))
   (list :timestamp-milli-seconds (+ milli-seconds 250)
	 :midi-events (list
		       (cl-synthesizer-midi-event:make-note-off-event 1 75 100)))))

  
(defun make-midi-events (duration-seconds)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((events nil) (milli-seconds (* 1000 duration-seconds)))
    (dotimes (i (/ milli-seconds 500))
      (let ((segment (make-midi-event-segment (* i 500))))
	(setf events (concatenate 'list events segment))))
    ;;(format t "~%Events: ~a~%" events)
    events))


(defun make-test-rack (&key duration-seconds voice-count)
  ""
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))

    ;; Add voices
    (dotimes (i voice-count)
      (cl-synthesizer:add-module rack (format nil "VOICE-~a" i) #'make-voice))
    
    (cl-synthesizer:add-module
     rack "MIDI-IFC" #'cl-synthesizer-modules-midi-interface:make-module :voice-count voice-count)
    
    (cl-synthesizer:add-module
     rack
     "MIDI-SEQUENCER"
     #'cl-synthesizer-modules-midi-sequencer:make-module :events
     (make-midi-events duration-seconds))

    (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-IFC" :midi-events)
    
    (dotimes (i voice-count)
      (cl-synthesizer:add-patch
       rack
       "MIDI-IFC"
       (cl-synthesizer-macro-util:make-keyword "CV" i) (format nil "VOICE-~a" i) :cv)
      (cl-synthesizer:add-patch
       rack
       "MIDI-IFC"
       (cl-synthesizer-macro-util:make-keyword "GATE" i) (format nil "VOICE-~a" i) :gate))

    (cl-synthesizer:add-module
     rack
     "MIXER"
     #'cl-synthesizer-modules-mixer:make-module
     :channel-count voice-count
     :channel-cv-max 5.0
     :channel-cv-gain 5.0
     :main-cv-max 5.0
     :main-cv-gain 2.5)
    
    (dotimes (i voice-count)
      (cl-synthesizer:add-patch
       rack
       (format nil "VOICE-~a" i) :audio
       "MIXER" (cl-synthesizer-macro-util:make-keyword "channel" i)))

    rack))

