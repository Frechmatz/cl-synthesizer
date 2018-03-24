(in-package :cl-synthesizer-midi-voice-manager)

;;
;; Voice
;;

(defclass voice ()
  ((notes :initform nil)))

(defun voice-is-note (cur-voice note)
  (find-if
   (lambda (i) (eq (first i) note))
   (slot-value cur-voice 'notes)))

;; Removes a note from the stack. Returns the current note or nil
(defun voice-remove-note (cur-voice note)
  (setf (slot-value cur-voice 'notes)
	(remove-if
	 (lambda (i) (eq (first i) note))
	 (slot-value cur-voice 'notes)))
  ;; return top note-number
  (first (first (slot-value cur-voice 'notes))))

;; Pushes a note. Returns the current note and the current stack size
(defun voice-push-note (cur-voice note &key (tick 0))
  (if (voice-is-note cur-voice note)
      ;; if note exists, move it to top
      (voice-remove-note cur-voice note))
  (progn
    (push (list note tick) (slot-value cur-voice 'notes))
    (values (first (first (slot-value cur-voice 'notes)))
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
   "Therefore notes are assigned to voices round-robin. Each voice has a stack of currently playing notes."))

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
(defun push-note (cur-voice-manager note)
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
(defun remove-note (cur-voice-manager note)
  (with-slots (voices) cur-voice-manager
    (let ((voice-index (voice-manager-find-voice-index-by-note cur-voice-manager note)))
      (if (not voice-index)
	  (values nil nil nil)
	  (values voice-index (voice-remove-note (elt voices voice-index) note) nil)))))

