(in-package :cl-synthesizer-modules-midi-monophonic-interface)

(defconstant +voice-state-cv+ 0)
(defconstant +voice-state-gate+ 1)
(defconstant +voice-state-gate-pending+ 2)

(defparameter +voice-states+
  '(+voice-state-cv+
    +voice-state-gate+
    +voice-state-gate-pending+))

(defun make-voice-state ()
  (let ((voice-state (make-array (length +voice-states+) :initial-element nil)))
    (setf (elt voice-state +voice-state-cv+) 0.0)
    (setf (elt voice-state +voice-state-gate+) 0.0)
    (setf (elt voice-state +voice-state-gate-pending+) nil)
    voice-state))

(defun get-voice-state-cv (state) (elt state +voice-state-cv+))
(defun set-voice-state-cv (state cv) (setf (elt state +voice-state-cv+) cv))
(defun get-voice-state-gate (state) (elt state +voice-state-gate+))
(defun set-voice-state-gate (state cv) (setf (elt state +voice-state-gate+) cv))
(defun get-voice-state-gate-pending (state) (elt state +voice-state-gate-pending+))
(defun set-voice-state-gate-pending (state pending) (setf (elt state +voice-state-gate-pending+) pending))

(defun make-module (name environment
		    &key
		      (channel nil)
		      (note-number-to-cv nil)
		      (cv-gate-on 5.0)
		      (cv-gate-off 0.0)
		      (force-gate-retrigger nil)
		      (stack-depth 5))
  "Creates a monophonic MIDI interface module. The module dispatches MIDI-Note events to a single voice. 
   If the voice is already assigned to a note, then the next note is pushed on top of the current note.
   The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
        <li>:stack-depth Maximum number of stacked notes.</li>
	<li>:channel Optional MIDI channel to which note events must belong. By default the
	    channel is ignored.</li>
	<li>:note-number-to-cv An optional function that is called with a MIDI note number
	    and returns a control-voltage. The default implementation is cv = note-number / 12.0</li>
	<li>:cv-gate-on The \"Gate on\" control voltage.</li>
	<li>:cv-gate-off The \"Gate off\" control voltage.</li>
	<li>:force-gate-retrigger If t then each note on event will cause a retriggering 
            of the gate signal. Otherwise the gate signal will just stay on when it is already on.</li>
    </ul>
    Gate transitions are implemented as follows: Incoming notes are stacked. The first note causes
    the gate signal to switch to On. Further \"nested\" note-on events only result
    in a change of the CV output but the gate signal will stay On.
    This behaviour can be overridden with the :force-gate-retrigger parameter.</li>
    </ul>
    The module has the following inputs:
    <ul>
	<li>:midi-events A list of MIDI events.</li>
    </ul>
    The module has the following outputs:
    <ul>
	<li>:gate-1 The gate signal.</li>
	<li>:cv-1 The control voltage representing the note which is on top of the note stack.</li>
    </ul></b>"
  (declare (ignore environment name))
  (if (not note-number-to-cv)
      (setf note-number-to-cv (lambda (note-number) (the single-float (/ note-number 12.0)))))
  (let* ((outputs nil)
	 (inputs nil)
	 (input-midi-events nil)
	 (voice-state (make-voice-state))
	 (voice-manager (make-instance 'cl-synthesizer-lru-set:lru-set :capacity stack-depth)))

    ;; Set up outputs
    (let ((cv-socket (cl-synthesizer-macro-util:make-keyword "CV" 0))
	  (gate-socket (cl-synthesizer-macro-util:make-keyword "GATE" 0)))
      (push (lambda () (get-voice-state-cv voice-state)) outputs)
      (push cv-socket outputs)
      (push (lambda () (get-voice-state-gate voice-state)) outputs)
      (push gate-socket outputs))

    ;; Set up inputs
    (setf inputs (list :midi-events (lambda(value) (setf input-midi-events value))))

    (labels ((retrigger-gate (voice-state)
	       "Put gate down for one tick."
	       (set-voice-state-gate voice-state cv-gate-off)
	       (set-voice-state-gate-pending voice-state t))
	     (activate-pending-gate ()
	       "Put pending gate up."
	       (if (get-voice-state-gate-pending voice-state)
		   (progn
		     (set-voice-state-gate voice-state cv-gate-on)
		     (set-voice-state-gate-pending voice-state nil))))
	     (activate-gate ()
	       "Set gate to up. If its already up then depending on state put it down for one tick."
	       (cond
		 ((= cv-gate-off (get-voice-state-gate voice-state))
		  (set-voice-state-gate voice-state cv-gate-on))
		 (t
		  (if force-gate-retrigger
		      (retrigger-gate voice-state))))
	       ))
      (list
       :inputs (lambda () inputs)
       :outputs (lambda () outputs)
       :update (lambda ()
		 ;; Pull up pending gate
		 (activate-pending-gate)
		 ;; Update voice
		 (dolist (midi-event input-midi-events)
		   (if (and midi-event
			    (or (not channel)
				(= channel (cl-synthesizer-midi-event:get-channel midi-event))))
		       (cond
			 ;; Note on
			 ((cl-synthesizer-midi-event:note-on-eventp midi-event)
			  (let ((note-number (cl-synthesizer-midi-event:get-note-number midi-event)))
			    (cl-synthesizer-lru-set:push-value voice-manager note-number)
			    (activate-gate)
			    (set-voice-state-cv voice-state  (funcall note-number-to-cv note-number))))
			 ;; Note off
			 ((cl-synthesizer-midi-event:note-off-eventp midi-event)
			  (cl-synthesizer-lru-set:remove-value
			   voice-manager
			   (cl-synthesizer-midi-event:get-note-number midi-event))
			  (let ((voice-note (cl-synthesizer-lru-set:current-value voice-manager)))
			    ;; if no note left then set gate to off
			    (if (not voice-note)
				(set-voice-state-gate voice-state cv-gate-off)
				(set-voice-state-cv
				 voice-state
				 (funcall note-number-to-cv voice-note)))))))))))))
