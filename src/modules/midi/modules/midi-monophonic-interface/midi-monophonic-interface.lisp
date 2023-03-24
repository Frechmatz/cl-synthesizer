(in-package :cl-synthesizer-modules-midi-monophonic-interface)

(defconstant +voice-state-cv+ 0)
(defconstant +voice-state-gate+ 1)
(defconstant +voice-state-gate-pending+ 2)
(defconstant +voice-state-velocity+ 3)

(defparameter +voice-states+
  '(+voice-state-cv+
    +voice-state-gate+
    +voice-state-gate-pending+
    +voice-state-velocity+))

(defun make-voice-state ()
  (let ((voice-state (make-array (length +voice-states+) :initial-element nil)))
    (setf (elt voice-state +voice-state-cv+) 0.0)
    (setf (elt voice-state +voice-state-gate+) 0.0)
    (setf (elt voice-state +voice-state-gate-pending+) nil)
    (setf (elt voice-state +voice-state-velocity+) 0.0)
    voice-state))

(defun get-voice-state-cv (state) (elt state +voice-state-cv+))
(defun set-voice-state-cv (state cv) (setf (elt state +voice-state-cv+) cv))
(defun get-voice-state-gate (state) (elt state +voice-state-gate+))
(defun set-voice-state-gate (state cv) (setf (elt state +voice-state-gate+) cv))
(defun get-voice-state-gate-pending (state) (elt state +voice-state-gate-pending+))
(defun set-voice-state-gate-pending (state pending) (setf (elt state +voice-state-gate-pending+) pending))
(defun get-voice-state-velocity (state) (elt state +voice-state-velocity+))
(defun set-voice-state-velocity (state cv) (setf (elt state +voice-state-velocity+) cv))

(defun make-module (name environment
		    &key
		      (channel nil)
		      (cv-gate-on 5.0)
		      (cv-gate-off 0.0)
		      (force-gate-retrigger nil)
		      (stack-depth 5)
		      (cv-velocity-max 5.0)
		      (force-velocity-update nil))
  "Creates a monophonic MIDI interface module. The module dispatches MIDI-Note events to a single voice. 
   If the voice is already assigned to a note, then the incoming note is pushed on top of the current note.
   <p>The function has the following parameters:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
        <li>:stack-depth Maximum number of stacked notes.</li>
	<li>:channel Optional MIDI channel to which note events must belong. By default the
	    channel is ignored.</li>
	<li>:cv-gate-on The \"Gate on\" control voltage.</li>
	<li>:cv-gate-off The \"Gate off\" control voltage.</li>
	<li>:force-gate-retrigger If t then each \"note on\" event will cause a retriggering 
            of the gate signal. Otherwise the gate signal will stay on when it is already on.</li>
	<li>:cv-velocity-max Control voltage that represents the maximum velocity (Velocity = 127).</li>
	<li>:force-velocity-update If t then each \"note on\" event will cause an
            update of the velocity control voltage according to the velocity of the current event.</li>
    </ul>
    Gate transitions are implemented as follows: Incoming notes are stacked. The 
    first (\"initiating\") \"note on\" event causes the gate signal to go up. The gate goes 
    down when after a \"note off\" event no more notes are on the stack.</p>
    <p>The module has the following inputs:
    <ul>
	<li>:midi-events A list of MIDI events.</li>
    </ul></p>
    <p>The module has the following outputs:
    <ul>
	<li>:gate The gate signal.</li>
	<li>:cv Pitch control voltage representing the note which is on top of the note stack. cv = note-number / 12.0</li>
	<li>:velocity Velocity control voltage. The velocity control voltage represents 
        the velocity of the initiating \"note on\" event (Gate goes up). Nested \"note on\" 
        events do not cause an update of this voltage. This behaviour can be overridden 
        by the :force-velocity-update parameter.</li>
    </ul></p>"
  (declare (ignore environment))
  (if (not cv-velocity-max)
      (error
       'cl-synthesizer:assembly-error
       :format-control "cv-velocity-max of MIDI-Monophonic-Interface '~a' must not be nil"
       :format-arguments (list name)))
  (if (<= cv-velocity-max 0.0)
      (error
       'cl-synthesizer:assembly-error
       :format-control "cv-velocity-max of MIDI-Monophonic-Interface '~a' must be greater than 0: '~a'"
       :format-arguments (list name cv-velocity-max)))
  (let* ((note-number-to-cv (lambda (note-number) (the single-float (/ note-number 12.0))))
	 (outputs nil)
	 (inputs nil)
	 (input-midi-events nil)
	 (voice-state (make-voice-state))
	 (voice-manager (make-instance 'cl-synthesizer-midi-lru-set::lru-set :capacity stack-depth)))

    ;; Set up outputs
    (push (list :get (lambda () (get-voice-state-cv voice-state))) outputs)
    (push :cv outputs)
    (push (list :get (lambda () (get-voice-state-gate voice-state))) outputs)
    (push :gate outputs)
    (push (list :get (lambda () (get-voice-state-velocity voice-state))) outputs)
    (push :velocity outputs)

    ;; Set up inputs
    (setf inputs (list :midi-events
		       (list :set (lambda(value) (setf input-midi-events value))
			     :get (lambda() input-midi-events))))

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
		      (retrigger-gate voice-state)))))
	     (velocity-to-cv (velocity)
	       "Velocity value (0..127) to CV"
	       (* velocity (/ cv-velocity-max 127.0))))
    (list
       :inputs (lambda() inputs)
       :outputs (lambda() outputs)
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
			  (let ((note-number (cl-synthesizer-midi-event:get-note-number midi-event))
				(first-note-p (cl-synthesizer-midi-lru-set::empty-p voice-manager)))
			    (cl-synthesizer-midi-lru-set::push-value voice-manager note-number)
			    (activate-gate)
			    (set-voice-state-cv voice-state  (funcall note-number-to-cv note-number))
			    (if (or first-note-p force-velocity-update)
				(set-voice-state-velocity
				 voice-state
				 (velocity-to-cv (cl-synthesizer-midi-event:get-velocity midi-event))))))
			 ;; Note off
			 ((cl-synthesizer-midi-event:note-off-eventp midi-event)
			  (cl-synthesizer-midi-lru-set::remove-value
			   voice-manager
			   (cl-synthesizer-midi-event:get-note-number midi-event))
			  (let ((voice-note (cl-synthesizer-midi-lru-set::current-value voice-manager)))
			    ;; if no note left then set gate to off
			    (if (not voice-note)
				(set-voice-state-gate voice-state cv-gate-off)
				(set-voice-state-cv
				 voice-state
				 (funcall note-number-to-cv voice-note)))))))))))))
