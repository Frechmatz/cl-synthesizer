;;
;;
;; A simple MIDI sequencer
;;
;;

(in-package :cl-synthesizer-modules-midi-sequencer)

(defun make-module (name environment &key events)
  "Creates a Midi-Sequencer module.
	The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:events A list of Midi events and their timestamps. Each
	    entry consists of a property list with the following keys:
	    <ul>
		<li>:timestamp-milli-seconds Point of time when events are to be fired. The very first
                timestamp of the synthesizer is 0.</li>
		<li>:midi-events List of Midi events to be fired.</li>
	    </ul>
	</li>
    </ul>
    The module has no inputs.
    The module has one output socket :midi-events."
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
