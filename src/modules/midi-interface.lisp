(in-package :cl-synthesizer-modules-midi-interface)

;;
;;
;; A Midi Rack Module
;;
;; Work in progress
;;


;;
;; Voice
;;

(defclass voice ()
  ((notes :initform nil)))

(defun voice-is-note (cur-voice note)
  (find note (slot-value cur-voice 'notes) :test #'eq))

;; Removes a note from the stack. Returns the current note or nil
(defun voice-remove-note (cur-voice note)
  (setf (slot-value cur-voice 'notes)
	(remove
	 note
	 (slot-value cur-voice 'notes)
	 :test #'eq))
  (first (slot-value cur-voice 'notes)))

;; Pushes a note. Returns the current note and the current stack size
(defun voice-push-note (cur-voice note)
  (if (voice-is-note cur-voice note)
      ;; if note exists, move it to top
      (voice-remove-note cur-voice note))
  (progn
    (push note (slot-value cur-voice 'notes))
    (values (first (slot-value cur-voice 'notes))
	    (length (slot-value cur-voice 'notes)))))

;;
;; Voice-Manager
;;

(defclass voice-manager ()
  ((voices :initform nil)
   (next-voice-index :initform 0))
  (:documentation
   "The voice-manager controls the assignment of note-events to voices. "
   "The allocation algorithm maximizes the time before a given voice is being assigned "
   "to a new note in order to not cut ('steal') the release phase of the audio output. "
   "Therefore voices are assigned round-robin. Each voice has a stack of currently playing notes."))

(defmethod initialize-instance :after ((mgr voice-manager) &key voice-count)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (if (eq 0 voice-count)
      (error "voice-mamager: voice-count must be greater zero"))
  (setf (slot-value mgr 'voices) (make-array voice-count))
  (dotimes (i voice-count)
    (setf (aref (slot-value mgr 'voices) i) (make-instance 'voice))))

(defun voice-manager-find-voice-index-by-note (cur-voice-manager note)
  (let ((found-index nil))
    (with-slots (voices) cur-voice-manager
      ;; todo: break out of dotimes when matching voice has been found
      (dotimes (i (length voices))
	(let ((v (elt voices i)))
	  (if (voice-is-note v note)
	      (setf found-index i)))))
    found-index))

;; Pushes a note.
;; Returns voice index, current voice note and the current stack size
(defun voice-manager-push-note (cur-voice-manager note)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (with-slots (voices next-voice-index) cur-voice-manager
    (let ((updated-voice-index next-voice-index))
      (let ((cur-voice (elt voices next-voice-index)))
	(multiple-value-bind (current-voice-note voice-note-stack-size)
	    (voice-push-note cur-voice note)
	  (setf next-voice-index (+ 1 next-voice-index))
	  (if (>= next-voice-index (length voices))
	      (setf next-voice-index 0))
	  (values updated-voice-index current-voice-note voice-note-stack-size))))))

;; Removes a note.
;; Returns voice index, current voice note and nil.
(defun voice-manager-remove-note (cur-voice-manager note)
  (with-slots (voices) cur-voice-manager
    (let ((voice-index (voice-manager-find-voice-index-by-note cur-voice-manager note)))
      (if (not voice-index)
	  (values nil nil nil)
	  (values voice-index (voice-remove-note (elt voices voice-index) note) nil)))))

;;
;; MIDI Interface
;;

(defconstant +voice-state-cv+ 0)
(defconstant +voice-state-gate+ 1)
(defconstant +voice-state-gate-on-logger+ 2)
(defconstant +voice-state-gate-off-logger+ 3)
(defconstant +voice-state-gate-trigger+ 4)

(defparameter +voice-states+
  '(+voice-state-cv+
    +voice-state-gate+
    +voice-state-gate-on-logger+
    +voice-state-gate-off-logger+
    +voice-state-gate-trigger+))

(defun make-voice-state (name environment voice-number)
  (let ((voice-state (make-array (length +voice-states+))))
    (setf (elt voice-state +voice-state-cv+) 0)
    (setf (elt voice-state +voice-state-gate+) 0)
    (setf (elt voice-state +voice-state-gate-on-logger+)
	  (funcall (getf environment :register-event) name (format nil "GATE-~a-ON" voice-number)))
    (setf (elt voice-state +voice-state-gate-off-logger+)
	  (funcall (getf environment :register-event) name (format nil "GATE-~a-OFF" voice-number)))
    (setf (elt voice-state +voice-state-gate-trigger+) nil)
    voice-state))

(defun midi-interface (name environment &key (voice-count 1))
  (let* ((current-controller 0)
	 (voice-states (make-array voice-count))
	 (output-socket-lookup-table (make-hash-table :test #'eq))
	 (voice-manager (make-instance 'voice-manager :voice-count voice-count))
	 (cv-keywords (cl-synthesizer-macro-util:make-keyword-list "CV" voice-count))
	 (gate-keywords (cl-synthesizer-macro-util:make-keyword-list "GATE" voice-count))
	 (controller-converter (cl-synthesizer-core:linear-converter
			       :input-min 0 :input-max 127 :output-min 0 :output-max 4.9))
	 (outputs (concatenate 'list '(:controller-1) cv-keywords gate-keywords)))
    (dotimes (i voice-count)
      (setf (elt voice-states i) (make-voice-state name environment i)))
    ;; Init Property Lookup Table
    (let ((i 0))
      (dolist (item cv-keywords)
	(setf (gethash item output-socket-lookup-table) (list i :CV))
	(setf i (+ 1 i))))
    (let ((i 0))
      (dolist (item gate-keywords)
	(setf (gethash item output-socket-lookup-table) (list i :GATE))
	(setf i (+ 1 i))))
    (setf (gethash :controller-1 output-socket-lookup-table) (list nil :CONTROLLER))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () '(:midi-event))
     :outputs (lambda () outputs)
     :get-output (lambda (output)
		   (let ((index (gethash output output-socket-lookup-table)))
		     (if (not index)
			 (error (format nil "Unknown input ~a requested from ~a" output name)))
		     (cond
		       ((eq :CONTROLLER (second index))
			current-controller)
		       ((eq :CV (second index))
			(elt (elt voice-states (first index)) +voice-state-cv+))
		       ((eq :GATE (second index))
			(elt (elt voice-states (first index)) +voice-state-gate+))
		       (t (error (format
				  nil
				  "Internal server error. Dont know how to handle input ~a requested from ~a"
				  output name))))))
     :update (lambda (&key (midi-event nil))
	       (if midi-event
		   (let ((event-type (first midi-event)))
		     (cond
		       ((eq event-type :cc)
			(setf current-controller
			      (funcall (getf controller-converter :input-to-output) (fourth midi-event))))
		       ((eq event-type :note-on)
			(multiple-value-bind (voice-index voice-note stack-size)
			    (voice-manager-push-note voice-manager (third midi-event))
			  (let ((voice-state (elt voice-states voice-index)))
			    (if (= 1 stack-size)
				(progn
				  (funcall (elt voice-state +voice-state-gate-on-logger+))
				  (setf (elt voice-state +voice-state-gate+) 5.0)))
			    (setf (elt voice-state +voice-state-cv+) (/ voice-note 12))
			    (format t "cv-oct: ~a~%" (elt voice-state +voice-state-cv+)))))
		       ((eq event-type :note-off)
			(multiple-value-bind (voice-index voice-note)
			    (voice-manager-remove-note voice-manager (third midi-event))
			  (if voice-index
			      (let ((voice-state (elt voice-states voice-index)))
				(if (not voice-note)
				    (progn
				      (funcall (elt voice-state +voice-state-gate-off-logger+))
				      (setf (elt voice-state +voice-state-gate+) 0))
				    (progn
				      (setf (elt voice-state +voice-state-cv+) (/ voice-note 12))
				    ))
			      (format t "cv-oct: ~a~%" (elt voice-state +voice-state-cv+))))))
			  )))))))
