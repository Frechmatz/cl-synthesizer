
(in-package :cl-synthesizer-midi-event)

(defun make-control-change-event (&key channel controller-number controller-value)
  "Creates a MIDI control change event."
  (list :type :cc :channel channel :controller-number controller-number :controller-value controller-value))

(defun make-note-on-event (&key channel note-number velocity)
  "Creates a MIDI Note-On event."
  (list :type :note-on :channel channel :note-number note-number :velocity velocity))

(defun make-note-off-event (&key channel note-number velocity)
  "Creates a MIDI Note-Off event."
  (list :type :note-off :channel channel :note-number note-number :velocity velocity))

(defun control-change-eventp (event)
  "Returns t if the given MIDI event is a Control-Change event."
  (eq (getf event :type) :cc))

(defun note-on-eventp (event)
  "Returns t if the given MIDI event is a Note-On event."
  (eq (getf event :type) :note-on))

(defun note-off-eventp (event)
  "Returns t if the given MIDI event is a Note-Off event."
  (eq (getf event :type) :note-off))

(defun get-channel (event)
  "Returns the MIDI channel number to which the event belongs."
  (getf event :channel))

(defun get-controller-number (event)
  "Returns the controller number of a Control-Change MIDI event."
  (getf event :controller-number))

(defun get-controller-value (event)
  "Returns the controller value of a Control-Change MIDI event."
  (getf event :controller-value))

(defun get-note-number (event)
  "Returns the note number of Note-On/Off MIDI event."
  (getf event :note-number))

(defun get-velocity (event)
  "Returns the velocity of a Note-On/Off MIDI event."
  (getf event :velocity))

