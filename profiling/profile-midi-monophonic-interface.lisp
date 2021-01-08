(defpackage :cl-synthesizer-profiling-midi-monophonic-interface
  (:use :cl))

(in-package :cl-synthesizer-profiling-midi-monophonic-interface)

(defun make-test-rack ()
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :input-sockets '(:midi-events))))
    (cl-synthesizer:add-module
     rack "MIDI-IFC"
     #'cl-synthesizer-modules-midi-monophonic-interface:make-module :force-gate-retrigger t)
    (cl-synthesizer:add-patch rack "INPUT" :midi-events "MIDI-IFC" :midi-events)
    rack))

(defun make-note-events (factory-fn)
  (let ((events nil))
    (dotimes (i 5)
      (push (funcall factory-fn :channel 1 :note-number (+ 75 i) :velocity 100) events)
      (push (funcall factory-fn :channel 1 :note-number (+ 75 i) :velocity 100) events))
    events))

(defun init ()
  (let ((rack (make-test-rack))
	(input-args-1 (make-note-events #'cl-synthesizer-midi-event:make-note-on-event))
	(input-args-2 (make-note-events #'cl-synthesizer-midi-event:make-note-off-event)))
    (lambda (&key duration-seconds)
      (let ((update-fn (getf rack :update)) (set-midi-events-fn (getf (funcall (getf rack :inputs)) :midi-events)))
	(dotimes (i (* 41000 duration-seconds))
	  (funcall set-midi-events-fn input-args-1)
	  (funcall update-fn)
	  (funcall set-midi-events-fn input-args-2)
	  (funcall update-fn))))))

