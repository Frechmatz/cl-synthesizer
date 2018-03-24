
(in-package :cl-synthesizer-midi-event)

(defun make-control-change-event (channel control-number value)
  (list :cc channel control-number value))

(defun make-note-on-event (channel note-number velocity)
  (list :note-on channel note-number velocity))

(defun make-note-off-event (channel note-number velocity)
  (list :note-off channel note-number velocity))

(defun control-change-eventp (event)
  (eq (first event) :cc))

(defun note-on-eventp (event)
  (eq (first event) :note-on))

(defun note-off-eventp (event)
  (eq (first event) :note-off))

(defun get-channel (event)
  (second event))

(defun get-control-number (event)
  (third event))

(defun get-control-value (event)
  (fourth event))

(defun get-note-number (event)
  (third event))

(defun get-velocity (event)
  (fourth event))

