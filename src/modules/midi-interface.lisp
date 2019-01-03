(in-package :cl-synthesizer-modules-midi-interface)

;;
;;
;; A Midi Rack Module
;;

(defconstant +voice-state-cv+ 0)
(defconstant +voice-state-gate+ 1)

(defparameter +voice-states+
  '(+voice-state-cv+
    +voice-state-gate+))

(defun make-voice-state ()
  (let ((voice-state (make-array (length +voice-states+))))
    (setf (elt voice-state +voice-state-cv+) 0.0)
    (setf (elt voice-state +voice-state-gate+) 0.0)
    voice-state))

(defun get-voice-state-cv (state) (elt state +voice-state-cv+))
(defun set-voice-state-cv (state cv) (setf (elt state +voice-state-cv+) cv))
(defun get-voice-state-gate (state) (elt state +voice-state-gate+))
(defun set-voice-state-gate (state cv) (setf (elt state +voice-state-gate+) cv))

(defun make-module (name environment
		       &key
			 (voice-count 1)
			 (channel nil)
			 (note-number-to-cv (lambda (note-number) (/ note-number 12)))
			 (play-mode :PLAY-MODE-POLY)
			 (cv-gate-on 5.0)
			 (cv-gate-off 0.0)
			 (force-gate-retrigger nil))
  "Creates a MIDI interface module. The module dispatches MIDI-Note events to so called voices where each
    voice is represented by a control-voltage and a gate signal. The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:voice-count The number of voices to be exposed by the module. Each voice consists
	    of the following output sockets:
	    <ul>
		<li>:gate-n The gate signal. n = 1..voice-count.</li>
		<li>:cv-n A control voltage representing the note number.
		    n = 1..voice-count.</li>
	    </ul>
        </li>
	<li>:channel Optional MIDI channel to which note events (not CC-Events) must belong. By default the
	    channel is ignored.</li>
	<li>:note-number-to-cv An optional function that is called with a MIDI note number
	    and returns a control-voltage.</li>
	<li>:play-mode
	    <ul>
		<li>:play-mode-poly Polyphonic play mode. Incoming note events will be
		    dispatched to \"available\" voices.</li>
		<li>:play-mode-unisono Monophonic play mode. All voices exposed by the module
		    are set to the current \"active\" note. Notes are stacked. When a note is
		    released, the voice outputs switch to the previous note.</li>
	    </ul>
            <p>
            The handling of play-modes is implemented by the package 
            cl-synthesizer-midi-voice-manager:voice-manager.
            </p>
        </li>
	<li>:cv-gate-on The \"Gate on\" control voltage.</li>
	<li>:cv-gate-off The \"Gate off\" control voltage.</li>
	<li>:force-gate-retrigger If t then in :play-mode-unisono play mode each note
	    event will cause a retriggering of the gate signal. Otherwise the gate signal
	    will just stay on when it is already on.</li>
    </ul>
    Gate transitions are implemented as follows:
    <ul>
	<li>In :play-mode-poly play mode each incoming note causes that the gate signal of the
	    assigned voice switches to On. If the gate signal of the assigned voice is already On
	    (this happens when the available voices are exhausted and a voice is \"stolen\") then
	    the gate signal switches to Off for the duration of one system tick and
	    then to On again.</li>
	<li>In :play-mode-unisono play mode incoming notes are stacked. The first note causes
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
	<li>:gate-1 ... :gate-n</li>
	<li>:cv-1 ... :cv-n</li>
    </ul>"
  (declare (ignore environment))
  (let* ((outputs nil)
	 (voice-states (make-array voice-count))
	 (output-socket-lookup-table (make-hash-table :test #'eq))
	 (pending-gate-on-voices nil)
	 (voice-manager (make-instance
			 'cl-synthesizer-midi-voice-manager:voice-manager
			 :voice-count (if (eq play-mode :PLAY-MODE-POLY) voice-count 1))))
    (dotimes (i voice-count)
      (let ((cv-socket (cl-synthesizer-macro-util:make-keyword "CV" i))
	    (gate-socket (cl-synthesizer-macro-util:make-keyword "GATE" i)))
	(setf outputs (push cv-socket outputs))
	(setf outputs (push gate-socket outputs))
	;; in Unisono mode all voices share the same state object
	(if (or (= 0 i) (not (eq play-mode :PLAY-MODE-UNISONO)))
	    (setf (elt voice-states i) (make-voice-state))
	    (setf (elt voice-states i) (elt voice-states 0)))
	(let ((cur-i i)) ;; new context
	  (setf (gethash cv-socket output-socket-lookup-table)
		(lambda () (get-voice-state-cv (elt voice-states cur-i))))
	  (setf (gethash gate-socket output-socket-lookup-table)
		(lambda () (get-voice-state-gate (elt voice-states cur-i)))))))
    (flet ((activate-gate (voice-index)
	     ;; set gate to on or if already on let it go down for one tick
	     (let ((voice-state (elt voice-states voice-index)))
	       (cond
		 ((= cv-gate-off (get-voice-state-gate voice-state))
		  (set-voice-state-gate voice-state cv-gate-on))
		 ((eq play-mode :PLAY-MODE-UNISONO)
		  (if force-gate-retrigger
		      (progn
			(set-voice-state-gate voice-state cv-gate-off)
			(push voice-index pending-gate-on-voices))))
		 (t
		  (set-voice-state-gate voice-state cv-gate-off)
		  (push voice-index pending-gate-on-voices))))))
    (list
     :inputs (lambda () '(:midi-events))
     :outputs (lambda () outputs)
     :get-output (lambda (output)
		   (let ((handler (gethash output output-socket-lookup-table)))
		     (if (not handler)
			 (error (format nil "Unknown input ~a requested from ~a" output name)))
		     (funcall handler)))
     :update (lambda (input-args)
	       ;; Set pending gates to on.
	       (dolist (voice-index pending-gate-on-voices)
		 (let ((voice-state (elt voice-states voice-index)))
		   (if (cl-synthesizer-midi-voice-manager:has-note voice-manager voice-index)
		       (set-voice-state-gate voice-state cv-gate-on))))
	       (setf pending-gate-on-voices nil)
	       ;; Update voices
	       (dolist (midi-event (getf input-args :midi-events))
		 (if (and midi-event
			  (or (not channel)
			      (= channel (cl-synthesizer-midi-event:get-channel midi-event))))
		     (cond
		       ;; Note on
		       ((cl-synthesizer-midi-event:note-on-eventp midi-event)
			(let ((voice-index
			       (cl-synthesizer-midi-voice-manager:push-note
				voice-manager
				(cl-synthesizer-midi-event:get-note-number midi-event))))
			  (let ((voice-state (elt voice-states voice-index)))
			    (activate-gate voice-index)
			    (set-voice-state-cv
			     voice-state
			     (funcall
			      note-number-to-cv
			      (cl-synthesizer-midi-event:get-note-number midi-event))))))
		       ;; Note off
		       ((cl-synthesizer-midi-event:note-off-eventp midi-event)
			(multiple-value-bind (voice-index voice-note)
			    (cl-synthesizer-midi-voice-manager:remove-note
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
