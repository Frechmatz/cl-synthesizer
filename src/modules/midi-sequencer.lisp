;;
;;
;; A simple MIDI sequencer
;;
;; Work in progress
;;

(in-package :cl-synthesizer-modules-midi-sequencer)

(defun midi-sequencer (name environment &key events)
  "events: List of (:timestamp-milli-seconds time-ms :midi-events (list midi-event))"
  (declare (ignore name))
  (let ((lookup-hash (make-hash-table :test #'eq))
	(ticks-per-milli-second (/ (getf environment :sample-rate) 1000))
	(cur-tick -1)
	(cur-midi-events nil))
    (dolist (evt events)
      (let ((tick (floor (* ticks-per-milli-second (getf evt :timestamp-milli-seconds)))))
	(setf (gethash tick lookup-hash) (getf evt :midi-events))))
    (list
     :inputs (lambda () '())
     :outputs (lambda () '(:midi-events))
     :update (lambda()
	       (setf cur-tick (+ 1 cur-tick))
	       (setf cur-midi-events (gethash cur-tick lookup-hash)))
     :get-output (lambda (output)
		   (declare (ignore output))
		   cur-midi-events))))
