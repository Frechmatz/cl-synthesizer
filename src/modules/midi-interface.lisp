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
;; Returns voice index and current note of voice or nil, nil.
(defun voice-manager-remove-note (cur-voice-manager note)
  (with-slots (voices) cur-voice-manager
    (let ((voice-index (voice-manager-find-voice-index-by-note cur-voice-manager note)))
      (if (not voice-index)
	  (values nil nil nil)
	  (values voice-index (voice-remove-note (elt voices voice-index) note))))))

;;
;; MIDI Interface
;;

(defun midi-interface (name environment)
  (let ((current-controller 0)
	(current-gate 0)
	(current-cv-oct 0)
	(controller-converter (cl-synthesizer-core:linear-converter :input-min 0 :input-max 127 :output-min 0 :output-max 4.9))
	(gate-on-event (funcall (getf environment :register-event) name "GATE-ON"))
	(gate-off-event (funcall (getf environment :register-event) name "GATE-OFF")))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () '(:midi-event))
     :outputs (lambda () '(:gate :cv-oct :out-1))
     :get-output (lambda (output)
		   (cond
		     ((eq output :gate) current-gate)
		     ((eq output :cv-oct) current-cv-oct)
		     ((eq output :out-1) current-controller)
		     (t (error (format nil "Unknown input ~a requested from ~a" output name)))))
     :update (lambda (&key (midi-event nil))
	       (if midi-event
		   (let ((event-type (first midi-event)))
		     (cond
		       ((eq event-type :cc)
			(setf current-controller
			      (funcall (getf controller-converter :input-to-output) (fourth midi-event))))
		       ((eq event-type :note-on)
			(funcall gate-on-event)
			(setf current-gate 5.0)
			(let ((note-number (third midi-event)))
			  (setf current-cv-oct (/ note-number 12))
			  (format t "cv-oct: ~a~%" current-cv-oct)
			  )
			)
		       ((eq event-type :note-off)
			(funcall gate-off-event)
			(setf current-gate 0)))
		     (format t "Gate: ~a CV-Oct: ~a Controller: ~a~%" current-gate current-cv-oct current-controller)))))))
