(defpackage :cl-synthesizer-profiling-midi-interface
  (:use :cl))

(in-package :cl-synthesizer-profiling-midi-interface)


(defun make-test-rack (&key (play-mode :PLAY-MODE-POLY))
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :input-sockets '(:midi-events))))
    (cl-synthesizer:add-module rack "MIDI-IFC"
			       #'cl-synthesizer-modules-midi-interface:make-module :voice-count 10 :play-mode play-mode)
    (cl-synthesizer:add-patch rack "INPUT" :midi-events "MIDI-IFC" :midi-events)
    rack))

(defun init (&key midi-play-mode)
  (let ((rack (make-test-rack :play-mode midi-play-mode))
	(input-args-1 (list
		       (cl-synthesizer-midi-event:make-note-on-event 1 75 100)
		       (cl-synthesizer-midi-event:make-note-on-event 1 76 100)
		       (cl-synthesizer-midi-event:make-note-on-event 1 77 100)
		       (cl-synthesizer-midi-event:make-note-on-event 1 78 100)
		       (cl-synthesizer-midi-event:make-note-on-event 1 78 100)
		       (cl-synthesizer-midi-event:make-control-change-event 1 1 1)))
	(input-args-2 (list
		       (cl-synthesizer-midi-event:make-note-off-event 1 75 100)
		       (cl-synthesizer-midi-event:make-note-off-event 1 76 100)
		       (cl-synthesizer-midi-event:make-note-off-event 1 77 100)
		       (cl-synthesizer-midi-event:make-note-off-event 1 78 100)
		       (cl-synthesizer-midi-event:make-note-off-event 1 78 100)
		       (cl-synthesizer-midi-event:make-control-change-event 1 1 1))))
    (lambda (&key duration-seconds)
      (let ((update-fn (getf rack :update)) (set-midi-events-fn (getf (funcall (getf rack :inputs)) :midi-events)))
	(dotimes (i (* 41000 duration-seconds))
	  (funcall set-midi-events-fn input-args-1)
	  (funcall update-fn)
	  (funcall set-midi-events-fn input-args-2)
	  (funcall update-fn))))))

