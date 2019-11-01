(defpackage :cl-synthesizer-profiling-midi-interface
  (:use :cl))

(in-package :cl-synthesizer-profiling-midi-interface)

(defun make-test-rack (&key (play-mode :PLAY-MODE-POLY) voice-count)
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :input-sockets '(:midi-events))))
    (cl-synthesizer:add-module
     rack "MIDI-IFC"
     #'cl-synthesizer-modules-midi-interface:make-module :voice-count voice-count :play-mode play-mode
     :force-gate-retrigger t)
    (cl-synthesizer:add-patch rack "INPUT" :midi-events "MIDI-IFC" :midi-events)
    rack))

(defun make-note-events (factory-fn voice-count)
  (let ((events nil))
    ;; Add two times the amount of note events as supported by the Midi interface
    ;; to force it to steal voices and retrigger Gates.
    (dotimes (i (* 2 voice-count))
      (push (funcall factory-fn 1 (+ 75 i) 100) events)
      (push (funcall factory-fn 1 (+ 75 i) 100) events))
    events))

(defun init (&key midi-play-mode voice-count)
  (let ((rack (make-test-rack :play-mode midi-play-mode :voice-count voice-count))
	(input-args-1 (make-note-events #'cl-synthesizer-midi-event:make-note-on-event voice-count))
	(input-args-2 (make-note-events #'cl-synthesizer-midi-event:make-note-off-event voice-count)))
    (lambda (&key duration-seconds)
      (let ((update-fn (getf rack :update)) (set-midi-events-fn (getf (funcall (getf rack :inputs)) :midi-events)))
	(dotimes (i (* 41000 duration-seconds))
	  (funcall set-midi-events-fn input-args-1)
	  (funcall update-fn)
	  (funcall set-midi-events-fn input-args-2)
	  (funcall update-fn))))))

