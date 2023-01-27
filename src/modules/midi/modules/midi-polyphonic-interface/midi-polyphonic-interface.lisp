(in-package :cl-synthesizer-modules-midi-polyphonic-interface)

;;
;;
;; A Midi Rack Module
;;

(defun make-symbol-impl (name num package)
  (if num
      (intern (format nil "~a-~a" (string-upcase name) num) package)
      (intern (string-upcase name) package)))

(defun make-keyword (name num)
  (make-symbol-impl name num "KEYWORD"))

(defun make-keyword-list (name count)
  "Returns list of keywords ordered by number of keyword: (:<name>-1, :<name>-2, ..., <name>-<count>.
   The numbering starts by one."
  (let ((l nil))
    (dotimes (i count)
      (push (make-keyword name (+ i 1)) l))
    (nreverse l)))


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
		      (voice-count 1)
		      (channel nil)
		      (cv-gate-on 5.0)
		      (cv-gate-off 0.0)
		      (cv-velocity-max 5.0))
  "Creates a polyphonic MIDI interface module. The module dispatches MIDI-Note events to so called voices where each
    voice is represented by a pitch-control voltage, a velocity-control voltage and a gate signal.
    <p>The function has the following parameters:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:voice-count The number of voices to be exposed by the module.</li>
	<li>:channel Optional MIDI channel to which note events must belong. By default the
	    channel is ignored.</li>
	<li>:cv-gate-on The \"Gate on\" control voltage.</li>
	<li>:cv-gate-off The \"Gate off\" control voltage.</li>
	<li>:cv-velocity-max Control voltage that represents the maximum velocity of 127.</li>
    </ul>
    Gate transitions are implemented as follows: Each incoming \"note on\" event causes that the gate signal of the
    assigned voice switches to On. If the gate signal of the assigned voice is already On
    (this happens when the available voices are exhausted and a voice is \"stolen\") then
    the gate signal switches to Off for the duration of one system tick and then to On again.</p>
    <p>The module has the following inputs:
    <ul>
	<li>:midi-events A list of MIDI events.</li>
    </ul></p>
    <p>The module has the following outputs:
    <ul>
	<li>:gate-1 ... :gate-n Gate signal.</li>
	<li>:cv-1 ... :cv-n Pitch control voltage. cv = note-number / 12.0</li>
	<li>:velocity-1 ... :velocity-n Velocity control voltage.</li>
    </ul></p>"
  (declare (ignore environment))
  (if (not cv-velocity-max)
      (error
       'cl-synthesizer:assembly-error
       :format-control "cv-velocity-max of MIDI-Polyphonic-Interface '~a' must not be nil"
       :format-arguments (list name)))
  (if (<= cv-velocity-max 0.0)
      (error
       'cl-synthesizer:assembly-error
       :format-control
       "cv-velocity-max of MIDI-Polyphonic-Interface '~a' must be greater than 0: '~a'"
       :format-arguments (list name cv-velocity-max)))
  (let* ((note-number-to-cv (lambda (note-number) (the single-float (/ note-number 12.0))))
	 (outputs nil)
	 (inputs nil)
	 (input-midi-events nil)
	 (voice-states (make-array voice-count))
	 (pending-gates nil)
	 (voice-manager (make-instance 'cl-synthesizer-midi-lru-set::lru-set :capacity voice-count )))

    ;; Set up voice states
    (dotimes (i voice-count)
      (setf (elt voice-states i) (make-voice-state)))

    ;; Set up outputs
    (dotimes (i voice-count)
      (let ((cv-socket (make-keyword "CV" (+ i 1)))
	    (gate-socket (make-keyword "GATE" (+ i 1)))
	    (velocity-socket (make-keyword "VELOCITY" (+ i 1))))
	(let ((cur-i i)) ;; new context
	  (push (lambda () (get-voice-state-cv (elt voice-states cur-i))) outputs)
	  (push cv-socket outputs)
	  (push (lambda () (get-voice-state-gate (elt voice-states cur-i))) outputs)
	  (push gate-socket outputs)
	  (push (lambda () (get-voice-state-velocity (elt voice-states cur-i))) outputs)
	  (push velocity-socket outputs))))

    ;; Set up inputs
    (setf inputs (list :midi-events (lambda(value) (setf input-midi-events value))))

    (labels ((retrigger-gate (voice-state)
	       "Put gate down for one tick."
	       (set-voice-state-gate voice-state cv-gate-off)
	       (set-voice-state-gate-pending voice-state t)
	       (setf pending-gates t))
	     (activate-pending-gates ()
	       "Put pending gates up."
	       (if pending-gates
		   (progn
		     (dotimes (index voice-count)
		       (let ((voice-state (elt voice-states index)))
			 (if (get-voice-state-gate-pending voice-state)
			     (progn
			       (set-voice-state-gate voice-state cv-gate-on)
			       (set-voice-state-gate-pending voice-state nil)))))
		     (setf pending-gates nil))))
	     (activate-gate (voice-index)
	       "Set gate to up. If its already up then depending on state put it down for one tick."
	       (let ((voice-state (elt voice-states voice-index)))
		 (cond
		   ((= cv-gate-off (get-voice-state-gate voice-state))
		    (set-voice-state-gate voice-state cv-gate-on))
		   (t
		    (retrigger-gate voice-state)))))
	     (velocity-to-cv (velocity)
	       "Velocity value (0..127) to CV"
	       (* velocity (/ cv-velocity-max 127.0))))
      (list
       :inputs (lambda () inputs)
       :outputs (lambda () outputs)
       :update (lambda ()
		 ;; Pull up pending gates
		 (activate-pending-gates)
		 ;; Update voices
		 (dolist (midi-event input-midi-events)
		   (if (and midi-event
			    (or (not channel)
				(= channel (cl-synthesizer-midi-event:get-channel midi-event))))
		       (cond
			 ;; Note on
			 ((cl-synthesizer-midi-event:note-on-eventp midi-event)
			  (let ((voice-index
				 (cl-synthesizer-midi-lru-set::push-value
				  voice-manager
				  (cl-synthesizer-midi-event:get-note-number midi-event))))
			    (let ((voice-state (elt voice-states voice-index)))
			      (activate-gate voice-index)
			      (set-voice-state-cv
			       voice-state
			       (funcall
				note-number-to-cv
				(cl-synthesizer-midi-event:get-note-number midi-event)))
			      (set-voice-state-velocity
			       voice-state
			       (velocity-to-cv (cl-synthesizer-midi-event:get-velocity midi-event))))))
			 ;; Note off
			 ((cl-synthesizer-midi-event:note-off-eventp midi-event)
			  (multiple-value-bind (voice-index voice-note)
			      (cl-synthesizer-midi-lru-set::remove-value
			       voice-manager
			       (cl-synthesizer-midi-event:get-note-number midi-event))
			    (if voice-index
				(let ((voice-state (elt voice-states voice-index)))
				  ;; if no note left then set gate to off
				  (if (not voice-note)
				      (set-voice-state-gate voice-state cv-gate-off)
				      (set-voice-state-cv
				       voice-state
				       (funcall note-number-to-cv voice-note)))))))))))))))

