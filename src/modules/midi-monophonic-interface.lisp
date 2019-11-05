(in-package :cl-synthesizer-modules-midi-monophonic-interface)

;;
;;
;; A Midi Rack Module
;;


;;
;; Voice
;;

(defclass voice ()
  ((notes :initform nil) ;; nil, a note or a list of notes. A note may be of any type
   (tick-counter :initform nil)
   (tick :initform 0)))

(defmethod initialize-instance :after ((v voice) &key tick-counter)
  (setf (slot-value v 'tick-counter) tick-counter)
  (setf (slot-value v 'tick) (funcall tick-counter)))

(defun voice-is-note (cur-voice note)
  (with-slots (notes) cur-voice
    (cond
      ((not notes)
       nil)
      ((not (listp notes))
       (equal note notes))
      (t 
       (find-if
	(lambda (i) (equal i note))
	notes)))))

(defun voice-get-current-note (cur-voice)
  (with-slots (notes) cur-voice
    (cond
      ((not notes)
       nil)
      ((not (listp notes))
       notes)
      (t
       (first notes)))))

;; Removes a note from the stack. Returns the current note or nil
(defun voice-remove-note (cur-voice note)
  (setf (slot-value cur-voice 'tick) (funcall (slot-value cur-voice 'tick-counter)))
  (with-slots (notes) cur-voice
    (cond
      ((not notes)
       nil)
      ((not (listp notes))
       (setf notes nil))
      (t
       (setf notes
	     (remove-if
	      (lambda (i) (equal i note))
	      notes)))))
  ;; return top note-number
  (voice-get-current-note cur-voice))

;; Pushes a note. Returns the current note.
(defun voice-push-note (cur-voice note)
  (setf (slot-value cur-voice 'tick) (funcall (slot-value cur-voice 'tick-counter)))
  (with-slots (notes) cur-voice
    (cond
      ((not notes)
       (setf notes note))
      ((not (listp notes))
       (if (voice-is-note cur-voice note)
	   nil ;; Nothing to to
	   (progn
	     ;; Switch over to stack representation
	     (setf notes (list notes))
	     ;; and push
	     (push note notes))))
      (t
       (if (voice-is-note cur-voice note)
	   ;; if note exists, delete it (we want to move it to top)
	   (voice-remove-note cur-voice note))
       (push note notes))))
  (voice-get-current-note cur-voice))

(defun voice-get-tick (cur-voice)
  (slot-value cur-voice 'tick))

(defun voice-clear (cur-voice)
  (setf (slot-value cur-voice 'notes) nil))

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
   (next-voice-index :initform 0)
   (tick-counter :initform (make-tick-counter)))
  (:documentation
   "A voice-manager controls the assignment of notes to so called voices.
    Voices consist of an index, a current note and a stack of \"pushed back\"
    notes. Voice-Managers are instantiated with the number of available
    voices. They do not keep states such as control voltage or gate.
    Notes can be represented by any objects that can be compared via #'equal.
    The class provides the following functions:
    <ul>
	<li>push-note (mgr note) Adds a note to the manager. The function decides
	    which voice to use and returns the index of the chosen voice.</li>
	<li>remove-note (mgr note) Removes a note from the manager. Returns
	    the index of the voice from which the note has been removed and
	    the current note of the voice. The current note may be not nil
	    if the voice manager has only one voice because in this case incoming
	    notes are stacked.</li>
	<li>has-note (mgr voice-index) Returns t if the given voice is assigned
	    to a note.</li>
    </ul>
    The constructor has the following arguments:
    <ul>
	<li>:voice-count Number of voices.</li>
    </ul>
    Example:
    <pre><code>
    (let ((voice-manager
             (make-instance
                 'cl-synthesizer-midi-voice-manager:voice-manager
                 :voice-count 5)))
         (let ((voice-index (push-note voice-manager 64)))))
    </code></pre>"))

(defmacro with-voice (mgr-voice index voice &body body)
  (let ((v (gensym)))
    `(let* ((,v ,mgr-voice)
	    (,index (first ,v)) (,voice (second ,v)))
     ,@body)))

(defmacro with-voices (voice-manager index voice voice-entry &body body)
  (let ((v (gensym)))
    `(dolist (,v (slot-value ,voice-manager 'voices))
       (let ((,index (first ,v)) (,voice (second ,v)) (,voice-entry ,v))
	 ,@body))))

(defmethod initialize-instance :after ((mgr voice-manager) &key voice-count)
  (if (equal 0 voice-count)
      (error "voice-manager: voice-count must be greater zero"))
  (dotimes (i voice-count)
    (push (list i (make-instance 'voice :tick-counter (slot-value mgr 'tick-counter))) (slot-value mgr 'voices)))
  (setf (slot-value mgr 'voices) (reverse (slot-value mgr 'voices))))

(defun voice-manager-find-voice-by-note (cur-voice-manager note)
  (let ((found-voice nil))
    (with-voices cur-voice-manager index voice voice-entry
      (declare (ignore index))
      (if (voice-is-note voice note)
	  (setf found-voice voice-entry)))
    found-voice))

(defun voice-manager-allocate-voice (cur-voice-manager)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((voices (slot-value cur-voice-manager 'voices)))
    (if (= 1 (length voices))
	;; in single voice mode return voice but do not reset it
	(first voices)
	(let* ((resulting-voice-entry nil)
	       (resulting-voice nil))
	  ;; get least recently used un-allocated voice
	  (let ((min-tick nil))
	    (with-voices cur-voice-manager index voice voice-entry
	      (declare (ignore index))
	      (if (not (voice-get-current-note voice))
		  (let ((tick (voice-get-tick voice)))
		    (if (or (not min-tick) (< tick min-tick))
			(progn
			  (setf min-tick tick)
			  (setf resulting-voice voice)
			  (setf resulting-voice-entry voice-entry)))))))
	  ;; if no un-allocated voice found then get least recently used voice
	  (if (not resulting-voice)
	      (let ((min-tick nil))
		(with-voices cur-voice-manager index voice voice-entry
		  (declare (ignore index))
		  (let ((tick (voice-get-tick voice)))
		    (if (or (not min-tick) (< tick min-tick))
			(progn
			  (setf min-tick tick)
			  (setf resulting-voice voice)
			  (setf resulting-voice-entry voice-entry)))))))
	  ;; in polyphonic mode, steal the voice (by resetting it)
	  (voice-clear resulting-voice)
	  resulting-voice-entry))))

;; Pushes a note.
;; Returns voice index
(defun push-note (cur-voice-manager note)
  (let ((cur-voice (voice-manager-allocate-voice cur-voice-manager)))
    (with-voice cur-voice index voice
      (voice-push-note voice note)
       index)))

;; Removes a note.
;; Returns voice index and current note.
(defun remove-note (cur-voice-manager note)
  (let ((found-voice (voice-manager-find-voice-by-note cur-voice-manager note)))
    (with-voice found-voice index voice
      (if (not voice)
	  (values nil nil)
	  (values
	   index
	   (voice-remove-note voice note))))))

;; Returns t if the given voice-index is assigned to at least one note
(defun has-note (cur-voice-manager voice-index)
  (with-voice (nth voice-index (slot-value cur-voice-manager 'voices)) index voice
    (declare (ignore index))
    (voice-get-current-note voice)))


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
			 (channel nil)
			 (note-number-to-cv nil)
			 (cv-gate-on 5.0)
			 (cv-gate-off 0.0)
			 (force-gate-retrigger nil))
  "Creates a MIDI interface module. The module dispatches MIDI-Note events to so called voices where each
    voice is represented by a control-voltage and a gate signal. The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:channel Optional MIDI channel to which note events must belong. By default the
	    channel is ignored.</li>
	<li>:note-number-to-cv An optional function that is called with a MIDI note number
	    and returns a control-voltage. The default implementation is cv = note-number / 12.0</li>
	<li>:cv-gate-on The \"Gate on\" control voltage.</li>
	<li>:cv-gate-off The \"Gate off\" control voltage.</li>
	<li>:force-gate-retrigger If t then in :play-mode-unisono play mode each note
	    event will cause a retriggering of the gate signal. Otherwise the gate signal
	    will just stay on when it is already on.</li>
    </ul>
    Gate transitions are implemented as follows:
    <ul>
	<li>Incoming notes are stacked. The first note causes
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
	<li>:gate-1</li>
	<li>:cv-1</li>
    </ul>
    For an example see <b>midi-sequencer</b>"
  (declare (ignore environment))
  (let ((play-mode :PLAY-MODE-UNISONO) (voice-count 1))
    (if (not note-number-to-cv)
	(setf note-number-to-cv (lambda (note-number) (the single-float (/ note-number 12.0)))))
    (let* ((outputs nil)
	   (inputs nil)
	   (input-midi-events nil)
	   (voice-states (make-array voice-count))
	   (pending-gates nil)
	   (voice-manager (make-instance
			   'voice-manager
			   :voice-count (if (eq play-mode :PLAY-MODE-POLY) voice-count 1))))
      ;; Set up voice states
      (dotimes (i voice-count)
	(if (= i 0)
	    (setf (elt voice-states 0) (make-voice-state))
	    ;; in Unisono mode all voices share the same state object
	    (if (eq play-mode :PLAY-MODE-UNISONO)
		(setf (elt voice-states i) (elt voice-states 0))
		(setf (elt voice-states i) (make-voice-state)))))

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
		     ((eq play-mode :PLAY-MODE-UNISONO)
		      (if force-gate-retrigger
			  (retrigger-gate voice-state)))
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
					 (funcall note-number-to-cv voice-note))))))))))))))))
