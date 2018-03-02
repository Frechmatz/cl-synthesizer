(in-package :cl-synthesizer-modules-midi-interface)

;;
;;
;; A Midi Rack Module
;;
;; Work in progress
;;

(defclass voice ()
  ((notes :initform nil)))

(defgeneric push-note (voice note))
(defgeneric remove-note (voice note))
(defgeneric get-note (voice))

(defmethod push-note ((cur-voice voice) note)
  (push note (slot-value cur-voice 'notes)))

(defmethod get-note ((cur-voice voice))
  (first (slot-value cur-voice 'notes)))

(defmethod remove-note ((cur-voice voice) note)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (setf (slot-value cur-voice 'notes)
	(remove
	 note
	 (slot-value cur-voice 'notes)
	 :test #'eq)))

(defclass voice-manager ()
  ((voices :initform nil)
   (next-voice-index :initform 0))
  (:documentation
   "The voice-manager controls the assignment of note-events to voices. A voice represents "
   "a specific CV/Gate output of the Midi interface. "
   "The allocation algorithm maximizes the time before a given voice is being assigned "
   "to a new note in order to not cut ('steal') the release phase of the audio output. "
   "Voices are assigned round-robin. Each voice has a stack of currently playing notes."))

(defgeneric push-note (voice-manager note))
(defgeneric remove-note (voice-manager note))
(defgeneric get-voice-note (voice-manager voice-number))

(defmethod initialize-instance :after ((mgr voice-manager) &key voice-count)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (if (eq 0 voice-count)
      (error "voice-mamager: voice-count must be greater zero"))
  (setf (slot-value mgr 'voices) (make-array voice-count))
  (dotimes (i voice-count)
    (setf (aref (slot-value mgr 'voices) i) (make-instance 'voice))))

(defmethod push-note ((cur-voice-manager voice-manager) note)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (with-slots (voices next-voice-index) cur-voice-manager
    (let ((cur-voice (elt voices next-voice-index)))
      (push-note cur-voice note)
      (setf next-voice-index (+ 1 next-voice-index))
      (if (>= next-voice-index (length voices))
	  (setf next-voice-index 0)))))

(defmethod remove-note ((cur-voice-manager voice-manager) note)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (with-slots (voices) cur-voice-manager
    (dotimes (i (length voices))
      (remove-note (elt voices i) note))))

(defmethod get-voice-note ((cur-voice-manager voice-manager) voice-number)
  (with-slots (voices) cur-voice-manager
    (if (>= voice-number (length voices))
	nil
	(let ((voice (elt voices voice-number)))
	  (get-note voice)))))

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
