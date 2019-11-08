(in-package :cl-synthesizer-modules-midi-polyphonic-interface)

;;
;;
;; A Midi Rack Module
;;


;;
;; Voice
;;

(defclass voice ()
  ((notes :initform nil) ;; nil or a note
   (tick-counter :initform nil)
   (tick :initform 0)))

(defmethod initialize-instance :after ((v voice) &key tick-counter)
  (setf (slot-value v 'tick-counter) tick-counter)
  (setf (slot-value v 'tick) (funcall tick-counter)))

(defmacro voice-is-note (cur-voice note)
  `(equal ,note (slot-value ,cur-voice 'notes)))

(defmacro voice-get-current-note (cur-voice)
  `(slot-value ,cur-voice 'notes))

(defmacro voice-get-tick (cur-voice)
  `(slot-value ,cur-voice 'tick))

(defmacro voice-touch (cur-voice)
  `(setf (slot-value ,cur-voice 'tick) (funcall (slot-value ,cur-voice 'tick-counter))))

;; Sets a note.
(defun voice-set-note (cur-voice note)
  (voice-touch cur-voice)
  (setf (slot-value cur-voice 'notes) note)
  note)

;; Resets the voice. Returns the current note or nil
(defun voice-reset (cur-voice)
  (if (slot-value cur-voice 'notes)
      (progn
	(voice-touch cur-voice)
	(setf (slot-value cur-voice 'notes) nil)))
  nil)

;;
;; Voice-Manager
;;

(defun make-tick-counter ()
  (let ((tick 0))
    (lambda()
      (setf tick (+ 1 tick))
      tick)))

(defclass voice-manager ()
  ((voices :initform nil) ;; list of (index voice)
   (tick-counter :initform (make-tick-counter))))

(defmethod initialize-instance :after ((mgr voice-manager) &key voice-count)
  (if (equal 0 voice-count)
      (error "voice-manager: voice-count must be greater zero"))
  (let ((voice-array (make-array voice-count)))
    (setf (slot-value mgr 'voices) voice-array)
    (dotimes (i voice-count)
      (setf (aref voice-array i) (make-instance 'voice :tick-counter (slot-value mgr 'tick-counter))))))

;; Returns an index
(defun voice-manager-find-voice-by-note (cur-voice-manager note)
  (let ((voices (slot-value cur-voice-manager 'voices)))
    (dotimes (index (length voices))
      (if (voice-is-note (elt voices index) note)
	  (return index)))))

;; Returns an index
(defun voice-manager-allocate-voice (cur-voice-manager)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((voices (slot-value cur-voice-manager 'voices))
	(min-tick-available-voice nil)
	(min-tick-playing-voice nil)
	(index-available-voice nil)
	(index-playing-voice nil))
    ;;
    ;; Collect
    ;; - the eldest available voice
    ;; - the eldest playing voice
    ;;
    (dotimes (index (length voices))
      (let ((voice (elt voices index)))
	(let ((tick (voice-get-tick voice)))
	  (if (not (voice-get-current-note voice))
	      ;; We've found an available voice
	      (if (or (not min-tick-available-voice) (< tick min-tick-available-voice))
		  (progn
		    (setf min-tick-available-voice tick)
		    (setf index-available-voice index)))
	      ;; Already playing voice
	      (if (or (not min-tick-playing-voice) (< tick min-tick-playing-voice))
		  (progn
		    (setf min-tick-playing-voice tick)
		    (setf index-playing-voice index)))))))
    (let ((resulting-index (if index-available-voice index-available-voice index-playing-voice)))
      (voice-reset (elt voices resulting-index))
      resulting-index)))

;; Pushes a note.
;; Returns the voice index
(defun push-note (cur-voice-manager note)
  (let ((index (voice-manager-allocate-voice cur-voice-manager)))
    (voice-set-note (elt (slot-value cur-voice-manager 'voices) index) note)
       index))

;; Removes a note.
;; Returns the index of the voice which has been cleared or nil
(defun remove-note (cur-voice-manager note)
  (let ((index (voice-manager-find-voice-by-note cur-voice-manager note)))
    (if index
	(voice-reset (elt (slot-value cur-voice-manager 'voices) index)))
    index))

;;
;; Midi-Interface
;;

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
		      (voice-count 1)
		      (channel nil)
		      (note-number-to-cv nil)
		      (cv-gate-on 5.0)
		      (cv-gate-off 0.0))
  "Creates a polyphonic MIDI interface module. The module dispatches MIDI-Note events to so called voices where each
    voice is represented by a control-voltage and a gate signal. The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:voice-count The number of voices to be exposed by the module.</li>
	<li>:channel Optional MIDI channel to which note events must belong. By default the
	    channel is ignored.</li>
	<li>:note-number-to-cv An optional function that is called with a MIDI note number
	    and returns a control-voltage. The default implementation is cv = note-number / 12.0</li>
	<li>:cv-gate-on The \"Gate on\" control voltage.</li>
	<li>:cv-gate-off The \"Gate off\" control voltage.</li>
    </ul>
    Gate transitions are implemented as follows: Each incoming note causes that the gate signal of the
    assigned voice switches to On. If the gate signal of the assigned voice is already On
    (this happens when the available voices are exhausted and a voice is \"stolen\") then
    the gate signal switches to Off for the duration of one system tick and then to On again.</br>
    The module has the following inputs:
    <ul>
	<li>:midi-events A list of MIDI events.</li>
    </ul>
    The module has the following outputs:
    <ul>
	<li>:gate-1 ... :gate-n Gates of the voices.</li>
	<li>:cv-1 ... :cv-n Control voltages of the voices.</li>
    </ul></b>"
  (declare (ignore environment name))
  (if (not note-number-to-cv)
      (setf note-number-to-cv (lambda (note-number) (the single-float (/ note-number 12.0)))))
  (let* ((outputs nil)
	 (inputs nil)
	 (input-midi-events nil)
	 (voice-states (make-array voice-count))
	 (pending-gates nil)
	 (voice-manager (make-instance 'voice-manager :voice-count voice-count )))
    ;; Set up voice states
    (dotimes (i voice-count)
      (setf (elt voice-states i) (make-voice-state)))

    ;; Set up outputs
    (dotimes (i voice-count)
      (let ((cv-socket (cl-synthesizer-macro-util:make-keyword "CV" i))
	    (gate-socket (cl-synthesizer-macro-util:make-keyword "GATE" i)))
	(let ((cur-i i)) ;; new context
	  (push (lambda () (get-voice-state-cv (elt voice-states cur-i))) outputs)
	  (push cv-socket outputs)
	  (push (lambda () (get-voice-state-gate (elt voice-states cur-i))) outputs)
	  (push gate-socket outputs))))

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
		    (retrigger-gate voice-state))))))
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
				 (push-note
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
			      (remove-note
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

