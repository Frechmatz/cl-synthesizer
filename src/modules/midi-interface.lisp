(in-package :cl-synthesizer-modules-midi-interface)

;;
;;
;; A Midi Rack Module
;;
;; Work in progress
;;



;;
;; MIDI Interface
;;

(defconstant +voice-state-cv+ 0)
(defconstant +voice-state-gate+ 1)
(defconstant +voice-state-gate-on-logger+ 2)
(defconstant +voice-state-gate-off-logger+ 3)
(defconstant +voice-state-gate-retrigger+ 4)

(defparameter +voice-states+
  '(+voice-state-cv+
    +voice-state-gate+
    +voice-state-gate-on-logger+
    +voice-state-gate-off-logger+
    +voice-state-gate-retrigger+))

(defun make-voice-state (name environment voice-number)
  (let ((voice-state (make-array (length +voice-states+))))
    (setf (elt voice-state +voice-state-cv+) 0)
    (setf (elt voice-state +voice-state-gate+) 0)
    (setf (elt voice-state +voice-state-gate-on-logger+)
	  (funcall (getf environment :register-event) name (format nil "GATE-~a-ON" voice-number)))
    (setf (elt voice-state +voice-state-gate-off-logger+)
	  (funcall (getf environment :register-event) name (format nil "GATE-~a-OFF" voice-number)))
    (setf (elt voice-state +voice-state-gate-retrigger+) nil)
    voice-state))

(defun midi-interface (name environment &key
					  (voice-count 1)
					  (note-number-to-cv (lambda (note-number) (/ note-number 12)))
					  (play-mode :PLAY-MODE-POLY))
  "play-mode: :PLAY-MODE-POLY, :PLAY-MODE-UNISONO"
  (let* ((voice-states (make-array voice-count))
	 (output-socket-lookup-table (make-hash-table :test #'eq))
	 (voice-manager (make-instance 'cl-synthesizer-midi-voice-manager:voice-manager
				       :voice-count (if (eq play-mode :PLAY-MODE-POLY) voice-count 1)))
	 (outputs (concatenate 'list
			       (cl-synthesizer-macro-util:make-keyword-list "CV" voice-count)
			       (cl-synthesizer-macro-util:make-keyword-list "GATE" voice-count))))
    (dotimes (i voice-count)
      (if (or (eq 0 i) (not (eq play-mode :PLAY-MODE-UNISONO)))
	  (setf (elt voice-states i) (make-voice-state name environment i))
	  (setf (elt voice-states i) (elt voice-states 0)))
      (let ((cur-i i)) ;; new context for the lambdas
	(setf (gethash (cl-synthesizer-macro-util:make-keyword "CV" cur-i) output-socket-lookup-table)
	      (lambda () (elt (elt voice-states cur-i) +voice-state-cv+)))
	(setf (gethash (cl-synthesizer-macro-util:make-keyword "GATE" cur-i) output-socket-lookup-table)
	      (lambda () (elt (elt voice-states cur-i) +voice-state-gate+)))))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () '(:midi-event))
     :outputs (lambda () outputs)
     :get-output (lambda (output)
		   (let ((handler (gethash output output-socket-lookup-table)))
		     (if (not handler)
			 (error (format nil "Unknown input ~a requested from ~a" output name)))
		     (funcall handler)))
     :update (lambda (&key (midi-event nil))
	       ;;(declare (optimize (debug 3) (speed 0) (space 0)))
	       ;;(break)
	       (if midi-event
		   (cond
		     ((cl-synthesizer-midi-event:note-on-eventp midi-event)
		      (multiple-value-bind (voice-index voice-note stack-size)
			  (cl-synthesizer-midi-voice-manager:push-note voice-manager (cl-synthesizer-midi-event:get-note-number midi-event))
			(let ((voice-state (elt voice-states voice-index)))
			  (if (= 1 stack-size)
			      (progn
				(funcall (elt voice-state +voice-state-gate-on-logger+))
				(setf (elt voice-state +voice-state-gate+) 5.0)))
			  (setf (elt voice-state +voice-state-cv+) (funcall note-number-to-cv voice-note))
			  (format t "cv-oct: ~a~%" (elt voice-state +voice-state-cv+)))))
		     ((cl-synthesizer-midi-event:note-off-eventp midi-event)
		      (multiple-value-bind (voice-index voice-note)
			  (cl-synthesizer-midi-voice-manager:remove-note voice-manager (cl-synthesizer-midi-event:get-note-number midi-event))
			(if voice-index
			    (let ((voice-state (elt voice-states voice-index)))
			      (if (not voice-note)
				  (progn
				    (funcall (elt voice-state +voice-state-gate-off-logger+))
				    (setf (elt voice-state +voice-state-gate+) 0))
				  (progn
				    (setf (elt voice-state +voice-state-cv+) (funcall note-number-to-cv voice-note))
				    ))
			      (format t "cv-oct: ~a~%" (elt voice-state +voice-state-cv+))))))
		     ))))))
