(defpackage :cl-synthesizer-profiling-midi-polyphonic-interface
  (:use :cl))

(in-package :cl-synthesizer-profiling-midi-polyphonic-interface)

(defun make-test-rack (&key voice-count)
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module
     rack "MIDI-IFC"
     #'cl-synthesizer-modules-midi-polyphonic-interface:make-module :voice-count voice-count)
    (cl-synthesizer:add-rack-input rack :midi-events "MIDI-IFC" :midi-events)
    rack))

(defun make-note-events (factory-fn voice-count)
  (let ((events nil))
    ;; Add two times the amount of note events as supported by the Midi interface
    ;; to force it to steal voices and retrigger Gates.
    (dotimes (i (* 2 voice-count))
      (push (funcall factory-fn :channel 1 :note-number (+ 75 i) :velocity 100) events)
      (push (funcall factory-fn :channel 1 :note-number (+ 75 i) :velocity 100) events))
    events))

(defun init (&key voice-count)
  (let ((rack (make-test-rack :voice-count voice-count))
	(input-args-1 (make-note-events #'cl-synthesizer-midi-event:make-note-on-event voice-count))
	(input-args-2 (make-note-events #'cl-synthesizer-midi-event:make-note-off-event voice-count)))
    (lambda (&key duration-seconds)
      (let ((update-fn (getf rack :update))
	    (set-midi-events-fn (getf (getf (funcall (getf rack :inputs)) :midi-events) :set)))
	(dotimes (i (* 41000 duration-seconds))
	  (funcall set-midi-events-fn input-args-1)
	  (funcall update-fn)
	  (funcall set-midi-events-fn input-args-2)
	  (funcall update-fn))))))

