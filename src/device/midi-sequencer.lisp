;;
;;
;; A simple and inefficient MIDI sequencer
;;
;; Work in progress
;;

(in-package :cl-synthesizer-device-midi-sequencer)

(defun midi-sequencer (name environment &key events)
  "events: List of (time-ms (list midi-event))"
  (declare (ignore name))
  (let ((lookup-hash (make-hash-table :test #'eq))
	(ticks-per-milli-second (/ (getf environment :sample-rate) 1000))
	(cur-tick -1))
    (dolist (evt events)
      (let ((tick (floor (* ticks-per-milli-second (first evt)))))
	(setf (gethash tick lookup-hash) (second evt))))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () '())
     :outputs (lambda () '(:midi-output))
     :get-output (lambda (output)
		   (declare (ignore output))
		   (setf cur-tick (+ 1 cur-tick))
		   (gethash cur-tick lookup-hash))
     :update (lambda () nil))))

