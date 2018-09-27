
(in-package :cl-synthesizer-midi-event)

(defun make-control-change-event (channel controller-number value)
  "Creates a MIDI control change event."
  (list :cc channel controller-number value))

(defun make-note-on-event (channel note-number velocity)
  "Creates a MIDI Note-On event."
  (list :note-on channel note-number velocity))

(defun make-note-off-event (channel note-number velocity)
  "Creates a MIDI Note-Off event."
  (list :note-off channel note-number velocity))

(defun control-change-eventp (event)
  "Returns t if the given MIDI event is a Control-Change event."
  (eq (first event) :cc))

(defun note-on-eventp (event)
  "Returns t if the given MIDI event is a Note-On event."
  (eq (first event) :note-on))

(defun note-off-eventp (event)
  "Returns t if the given MIDI event is a Note-Off event."
  (eq (first event) :note-off))

(defun get-channel (event)
  "Returns the MIDI channel number to which the event belongs."
  (second event))

(defun get-controller-number (event)
  "Returns the controller number of the MIDI event."
  (third event))

(defun get-controller-value (event)
  "Returns the controller value of the MIDI event."
  (fourth event))

(defun get-note-number (event)
  "Returns the note number of the MIDI event."
  (third event))

(defun get-velocity (event)
  "Returns the velocity of the MIDI event."
  (fourth event))

