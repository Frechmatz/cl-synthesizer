(defpackage :cl-synthesizer-profiling-keyboard
  (:use :cl))

(in-package :cl-synthesizer-profiling-keyboard)

(defun make-symbol-impl (name num package)
  (if num
      (intern (format nil "~a-~a" (string-upcase name) num) package)
      (intern (string-upcase name) package)))

(defun make-keyword (name num)
  (make-symbol-impl name num "KEYWORD"))

(defun make-voice (name environment &key exponential)
  (declare (ignore name))
  (let ((rack (cl-synthesizer:make-rack
	       :environment environment)))
    
    (cl-synthesizer:add-module
     rack "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency (cl-synthesizer-midi:get-note-number-frequency 0)
     :v-peak 5.0
     :wave-forms '(:sine))

    (cl-synthesizer:add-module
     rack "ADSR"
     #'cl-synthesizer-modules-adsr:make-module
     :attack-time-ms 50 :attack-target-output 5.0
     :decay-time-ms 20 :decay-target-output 3.0
     :release-time-ms 30
     :backward-coupled t
     :exponential exponential)
    
    (cl-synthesizer:add-module
     rack
     "VCA"
     #'cl-synthesizer-modules-vca:make-module
     :cv-max 5.0
     :exponential exponential)

    (cl-synthesizer:add-patch rack "VCO" :sine "VCA" :input)
    (cl-synthesizer:add-patch rack "ADSR" :cv "VCA" :cv)
    (cl-synthesizer:add-rack-input rack :cv "VCO" :cv-exp)
    (cl-synthesizer:add-rack-input rack :gate "ADSR" :gate)
    (cl-synthesizer:add-rack-output rack :audio "VCA" :output)
    
    rack))


(defun make-midi-event-segment (milli-seconds)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (list
   (list :timestamp-milli-seconds milli-seconds
	 :midi-events (list
		       (cl-synthesizer-midi-event:make-note-on-event
			:channel 1
			:note-number 69
			:velocity 100)))
   (list :timestamp-milli-seconds (+ milli-seconds 100)
	 :midi-events (list
		       (cl-synthesizer-midi-event:make-note-off-event
			:channel 1
			:note-number 69
			:velocity 100)))
   (list :timestamp-milli-seconds (+ milli-seconds 200)
	 :midi-events (list
		       (cl-synthesizer-midi-event:make-note-on-event
			:channel 1
			:note-number 75
			:velocity 100)))
   (list :timestamp-milli-seconds (+ milli-seconds 250)
	 :midi-events (list
		       (cl-synthesizer-midi-event:make-note-off-event
			:channel 1
			:note-number 75
			:velocity 100)))))

  
(defun make-midi-events (duration-seconds)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((events nil) (milli-seconds (* 1000 duration-seconds)))
    (dotimes (i (/ milli-seconds 500))
      (let ((segment (make-midi-event-segment (* i 500))))
	(setf events (concatenate 'list events segment))))
    ;;(format t "~%Events: ~a~%" events)
    events))


(defun make-test-rack (&key duration-seconds voice-count exponential)
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))

    ;; Add voices
    (dotimes (i voice-count)
      (cl-synthesizer:add-module rack (format nil "VOICE-~a" i) #'make-voice :exponential exponential))
    
    (cl-synthesizer:add-module
     rack "MIDI-IFC" #'cl-synthesizer-modules-midi-polyphonic-interface:make-module :voice-count voice-count)
    
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
       (make-keyword "CV" (+ i 1)) (format nil "VOICE-~a" i) :cv)
      (cl-synthesizer:add-patch
       rack
       "MIDI-IFC"
       (make-keyword "GATE" (+ i 1)) (format nil "VOICE-~a" i) :gate))

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
       "MIXER" (make-keyword "channel" (+ i 1))))

    rack))

