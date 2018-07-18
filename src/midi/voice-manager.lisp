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
    (values (voice-get-current-note cur-voice)
	    (length (slot-value cur-voice 'notes)))))

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

(defun make-voice-manager-voice (index)
  (list index (make-instance 'voice)))

(defun get-voice-manager-voice-index (voice)
  (first voice))

(defun get-voice-manager-voice-voice (voice)
  (second voice))

(defmethod initialize-instance :after ((mgr voice-manager) &key voice-count)
  (if (eq 0 voice-count)
      (error "voice-manager: voice-count must be greater zero"))
  (dotimes (i voice-count)
    (push (make-voice-manager-voice i) (slot-value mgr 'voices))))

(defun voice-manager-find-voice-by-note (cur-voice-manager note)
  (let ((found-voice nil))
    (with-slots (voices) cur-voice-manager
      ;; todo: break out of dolist when matching voice has been found
      (dolist (v voices)
	(if (voice-is-note (get-voice-manager-voice-voice v) note)
	    (setf found-voice v))))
    found-voice))

(defun voice-manager-get-least-recently-used-voice (cur-voice-manager)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((voices (copy-list (slot-value cur-voice-manager 'voices))))
    (let ((sorted-voices
	   (sort
	    voices
	    (lambda (v1 v2)
	      ;; If the first argument is greater than or equal to the second then the predicate should return false.
	      (let ((sl1 (voice-get-stack-size (get-voice-manager-voice-voice v1)))
		    (sl2 (voice-get-stack-size (get-voice-manager-voice-voice v2))))
		(if (eq sl1 sl2)
		    (if (> (voice-get-tick (get-voice-manager-voice-voice v1))
			   (voice-get-tick (get-voice-manager-voice-voice v2)))
			nil
			t)
			 (if (> sl1 sl2) nil t)))))))
      (first sorted-voices))))

(defun voice-manager-allocate-voice (cur-voice-manager)
  (if (= 1 (length (slot-value cur-voice-manager 'voices)))
      ;; in single voice mode, keep state of voice
      (first (slot-value cur-voice-manager 'voices))
      (let ((v (voice-manager-get-least-recently-used-voice cur-voice-manager)))
	;; in polyphonic mode, steal the voice and reset it
	(voice-clear (get-voice-manager-voice-voice v))
	v)))
    
;; Pushes a note.
;; Returns voice index, current voice note and the current stack size
(defun push-note (cur-voice-manager note)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((cur-voice (voice-manager-allocate-voice cur-voice-manager)))
    (multiple-value-bind (current-voice-note voice-note-stack-size)
	(voice-push-note (second cur-voice) note)
      (values
       (get-voice-manager-voice-index cur-voice)
       current-voice-note
       voice-note-stack-size))))

;; Removes a note.
;; Returns voice index, current voice note and stack-size.
(defun remove-note (cur-voice-manager note)
  (with-slots (voices) cur-voice-manager
    (let ((voice (voice-manager-find-voice-by-note cur-voice-manager note)))
      (if (not voice)
	  (values nil nil nil)
	  (values
	   (get-voice-manager-voice-index voice)
	   (voice-remove-note (get-voice-manager-voice-voice voice) note)
	   (voice-get-stack-size (get-voice-manager-voice-voice voice)))))))

;; Returns t if the given voice-index is assigned to at least one note
(defun has-note (cur-voice-manager voice-index)
  (let* ((voices (slot-value cur-voice-manager 'voices))
	 (v (second (nth voice-index voices))))
    (if (voice-get-current-note v)
	t
	nil)))
