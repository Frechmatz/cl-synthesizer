(in-package :cl-synthesizer-midi-voice-manager)

;;
;; Voice
;;

(defparameter *tick* 0)

(defun next-tick ()
  (setf *tick* (+ 1 *tick*))
  *tick*)

(defclass voice ()
  ((notes :initform nil)
   (tick :initform 0)))

(defmethod initialize-instance :after ((v voice) &rest args)
  (declare (ignore args))
  (setf (slot-value v 'tick) (next-tick)))

(defun voice-is-note (cur-voice note)
  (find-if
   (lambda (i) (eq i note))
   (slot-value cur-voice 'notes)))

(defun voice-get-current-note (cur-voice)
   (first (slot-value cur-voice 'notes)))

;; Removes a note from the stack. Returns the current note or nil
(defun voice-remove-note (cur-voice note)
  (setf (slot-value cur-voice 'tick) (next-tick))
  (setf (slot-value cur-voice 'notes)
	(remove-if
	 (lambda (i) (eq i note))
	 (slot-value cur-voice 'notes)))
  ;; return top note-number
  (voice-get-current-note cur-voice))

;; Pushes a note. Returns the current note and the current stack size
(defun voice-push-note (cur-voice note)
  (setf (slot-value cur-voice 'tick) (next-tick))
  (if (voice-is-note cur-voice note)
      ;; if note exists, move it to top
      (voice-remove-note cur-voice note))
  (progn
    (push note (slot-value cur-voice 'notes))
    (values (voice-get-current-note cur-voice))))

(defun voice-get-tick (cur-voice)
  (slot-value cur-voice 'tick))

(defun voice-get-stack-size (cur-voice)
  (length (slot-value cur-voice 'notes)))

(defun voice-clear (cur-voice)
  (setf (slot-value cur-voice 'notes) nil))

;;
;; Voice-Manager
;;

(defclass voice-manager ()
  ((voices :initform nil) ;; list of (index voice)
   (next-voice-index :initform 0))
  (:documentation
   "The voice-manager controls the assignment of note-events to voices."))

(defmacro with-voice (mgr-voice index voice &body body)
  `(let ((,index (first ,mgr-voice)) (,voice (second ,mgr-voice)))
     ,@body))

(defmacro with-voices (voice-manager index voice voice-entry &body body)
  (let ((v (gensym)))
    `(dolist (,v (slot-value ,voice-manager 'voices))
       (let ((,index (first ,v)) (,voice (second ,v)) (,voice-entry ,v))
	 ,@body))))

(defun make-voice-manager-voice (index)
  (list index (make-instance 'voice)))

(defmethod initialize-instance :after ((mgr voice-manager) &key voice-count)
  (if (eq 0 voice-count)
      (error "voice-manager: voice-count must be greater zero"))
  (dotimes (i voice-count)
    (push (make-voice-manager-voice i) (slot-value mgr 'voices)))
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
	  (let ((min-tick 99999999))
	    (with-voices cur-voice-manager index voice voice-entry
	      (declare (ignore index))
	      (if (not (voice-get-current-note voice))
		  (let ((tick (voice-get-tick voice)))
		    (if (< tick min-tick)
			(progn
			  (setf min-tick tick)
			  (setf resulting-voice voice)
			  (setf resulting-voice-entry voice-entry)))))))
	  ;; if no voice found then get least recently used voice
	  (if (not resulting-voice)
	      (let ((min-tick 99999999))
		(with-voices cur-voice-manager index voice voice-entry
		  (declare (ignore index))
		  (let ((tick (voice-get-tick voice)))
		    (if (< tick min-tick)
			(progn
			  (setf min-tick tick)
			  (setf resulting-voice voice)
			  (setf resulting-voice-entry voice-entry)))))))
	  ;; in polyphonic mode, steal the voice (by resetting it)
	  (voice-clear resulting-voice)
	  resulting-voice-entry))))
    
;; Pushes a note.
;; Returns voice index, current voice note and the current stack size
(defun push-note (cur-voice-manager note)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((cur-voice (voice-manager-allocate-voice cur-voice-manager)))
    (with-voice cur-voice index voice
      (multiple-value-bind (current-voice-note)
	  (voice-push-note voice note)
	(values
	 index
	 current-voice-note)))))

;; Removes a note.
;; Returns voice index, current voice note and stack-size.
(defun remove-note (cur-voice-manager note)
  (with-slots (voices) cur-voice-manager
    (let ((found-voice (voice-manager-find-voice-by-note cur-voice-manager note)))
      (with-voice found-voice index voice
	(if (not voice)
	    (values nil nil nil)
	    (values
	     index
	     (voice-remove-note voice note)))))))

;; Returns t if the given voice-index is assigned to at least one note
(defun has-note (cur-voice-manager voice-index)
  (let* ((voices (slot-value cur-voice-manager 'voices))
	 (v (second (nth voice-index voices))))
    (if (voice-get-current-note v)
	t
	nil)))
