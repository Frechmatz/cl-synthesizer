;;
;;
;; A simple MIDI sequencer
;;
;;

(in-package :cl-synthesizer-modules-midi-sequencer)

(defun make-module (name environment &key events)
  "Creates a Midi-Sequencer module.
    <p>The function has the following parameters:
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
            The events must be ordered by timestamp and there must be no duplicate timestamps. 
	</li>
    </ul></p>
    The module has no inputs.
    The module has one output socket :midi-events."
  (let ((ticks-per-milli-second (/ (getf environment :sample-rate) 1000))
	(event-array (make-array (length events) :initial-element nil))
	(timestamp-array (make-array (length events) :element-type 'number))
	(cur-tick -1)
	(cur-midi-events nil)
	(cur-index 0))
    (flet ((get-event-timestamp (evt)
	     (floor (* ticks-per-milli-second (getf evt :timestamp-milli-seconds)))))
      (let ((i 0) (occupied-time-slots nil) (cur-timestamp -1))
	(dolist (evt events)
	  (let ((timestamp (get-event-timestamp evt)))
	    (if (< timestamp cur-timestamp)
		(cl-synthesizer:signal-invalid-arguments-error
		 :format-control "Events must be ordered by timestamp: ~a"
		 :format-arguments (list name)))
	    (if (find-if (lambda (item) (= item timestamp)) occupied-time-slots)
		(cl-synthesizer:signal-invalid-arguments-error
		 :format-control "Timestamps not unique: ~a"
		 :format-arguments (list name)))
	    (setf cur-timestamp timestamp)
	    (push timestamp occupied-time-slots)
	    (setf (aref event-array i) (getf evt :midi-events))
	    (setf (aref timestamp-array i) timestamp)
	    (setf i (+ i 1))))))
    (let ((outputs (list :midi-events (lambda() cur-midi-events))))
      (list
       :inputs (lambda() nil)
       :outputs (lambda() outputs)
       :update (lambda()
		 (setf cur-tick (+ 1 cur-tick))
		 (if (<= (length timestamp-array) cur-index)
		     (setf cur-midi-events nil)
		     (progn
		       (let ((cursor-timestamp (elt timestamp-array cur-index)))
			 (if (<= cursor-timestamp cur-tick)
			     (progn
			       (setf cur-midi-events (elt event-array cur-index))
			       (setf cur-index (+ 1 cur-index)))
			     (setf cur-midi-events nil))))))))))
