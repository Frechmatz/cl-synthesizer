(in-package :cl-synthesizer-lru-set)

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

(defclass lru-set ()
  ((voices :initform nil) ;; list of (index voice)
   (tick-counter :initform (make-tick-counter))))

(defmethod initialize-instance :after ((mgr lru-set) &key voice-count)
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


;; Returns a value
(defun current-note (cur-voice-manager)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((voices (slot-value cur-voice-manager 'voices))
	(max-tick-playing-voice nil)
	(index-playing-voice nil)
	(note-playing-voice nil))
    ;; get the newest playing voice
    (format t "~%current-note: length: ~a" (length voices))
    (dotimes (index (length voices))
      (let ((voice (elt voices index)))
	(let ((tick (voice-get-tick voice)))
	  (if (voice-get-current-note voice)
	      ;; Already playing voice
	      (if (or (not max-tick-playing-voice) (< max-tick-playing-voice tick))
		  (progn
		    (format t "~%Voice: Index: ~a Note: ~a Tick: ~a" index (voice-get-current-note voice) tick)
		    (setf note-playing-voice (voice-get-current-note voice))
		    (setf max-tick-playing-voice tick)
		    (setf index-playing-voice index)))))))
    note-playing-voice))

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


