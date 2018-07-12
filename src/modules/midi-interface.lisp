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
(defconstant +voice-state-gate-retrigger+ 2)

(defparameter +voice-states+
  '(+voice-state-cv+
    +voice-state-gate+
    +voice-state-gate-retrigger+))

(defun make-voice-state (name environment voice-number)
  (declare (ignore name environment voice-number))
  (let ((voice-state (make-array (length +voice-states+))))
    (setf (elt voice-state +voice-state-cv+) 0)
    (setf (elt voice-state +voice-state-gate+) 0)
    (setf (elt voice-state +voice-state-gate-retrigger+) nil)
    voice-state))

(defun get-voice-state-cv (state) (elt state +voice-state-cv+))
(defun set-voice-state-cv (state cv) (setf (elt state +voice-state-cv+) cv))
(defun get-voice-state-gate (state) (elt state +voice-state-gate+))
(defun set-voice-state-gate (state cv) (setf (elt state +voice-state-gate+) cv))
(defun get-voice-state-gate-retrigger (state) (elt state +voice-state-gate-retrigger+))
(defun set-voice-state-gate-retrigger (state cv) (setf (elt state +voice-state-gate-retrigger+) cv))

(defun validate-controller (controller module-outputs)
  (let ((output-keyword (getf controller :socket)))
    (if (not (keywordp output-keyword))
	(cl-synthesizer:signal-assembly-error
	 :format-control "Controller handler output socket ~a must be a keyword"
	 :format-arguments (list output-keyword)))
    (if (find output-keyword module-outputs)
	(cl-synthesizer:signal-assembly-error
	 :format-control "Controller handler output socket ~a is already defined"
	 :format-arguments (list output-keyword))))
  (if (or (not (listp (getf controller :handler))) (= 0 (length (getf controller :handler))))
      (cl-synthesizer:signal-assembly-error
       :format-control "Controller handler object ~a must be a non-empty list"
       :format-arguments (list controller)))
  (if (not (getf (getf controller :handler) :get-output))
      (cl-synthesizer:signal-assembly-error
       :format-control "Controller handler object ~a must provide a 'get-output' function property"
       :format-arguments (list controller)))
  (if (not (getf (getf controller :handler) :update))
      (cl-synthesizer:signal-assembly-error
       :format-control "Controller handler object ~a must provide a 'update' function property"
       :format-arguments (list controller))))

(defun midi-interface (name environment
		       &key
			 (voice-count 1)
			 (note-number-to-cv (lambda (note-number) (/ note-number 12)))
			 (play-mode :PLAY-MODE-POLY)
			 (cv-gate-on 5.0)
			 (cv-gate-off 0.0)
			 (controllers nil))
  "play-mode: :PLAY-MODE-POLY, :PLAY-MODE-UNISONO
   controllers: A list of controllers. Each entry consists of a list of
   :socket <output-keyword> :handler (list :update lambda (midi-events) () :get-output :lambda ()())"
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let* ((outputs nil)
	 (voice-states (make-array voice-count))
	 (output-socket-lookup-table (make-hash-table :test #'eq))
	 (voice-manager (make-instance
			 'cl-synthesizer-midi-voice-manager:voice-manager
			 :voice-count (if (eq play-mode :PLAY-MODE-POLY) voice-count 1))))
    (dotimes (i voice-count)
      (let ((cv-socket (cl-synthesizer-macro-util:make-keyword "CV" i))
	    (gate-socket (cl-synthesizer-macro-util:make-keyword "GATE" i)))
	(setf outputs (push cv-socket outputs))
	(setf outputs (push gate-socket outputs))
	;; in Unisono mode all voices share the same state object
	(if (or (= 0 i) (not (eq play-mode :PLAY-MODE-UNISONO)))
	    (setf (elt voice-states i) (make-voice-state name environment i))
	    (setf (elt voice-states i) (elt voice-states 0)))
	(let ((cur-i i)) ;; new context
	  (setf (gethash cv-socket output-socket-lookup-table)
		(lambda () (get-voice-state-cv (elt voice-states cur-i))))
	  (setf (gethash gate-socket output-socket-lookup-table)
		(lambda () (get-voice-state-gate (elt voice-states cur-i)))))))
    ;; process controllers
    (dolist (controller controllers)
      (validate-controller controller outputs)
      (setf outputs (push (getf controller :socket) outputs))
      (let ((cur-controller controller)) ;; new context
	(setf (gethash (getf cur-controller :socket) output-socket-lookup-table)
	      (lambda () (funcall (getf (getf cur-controller :handler) :get-output))))))
    (list
     :inputs (lambda () '(:midi-events))
     :outputs (lambda () outputs)
     :get-output (lambda (output)
		   (let ((handler (gethash output output-socket-lookup-table)))
		     (if (not handler)
			 (error (format nil "Unknown input ~a requested from ~a" output name)))
		     (funcall handler)))
     :update (lambda (&key (midi-events nil))
	       ;; Update controllers
	       (dolist (c controllers)
		 (funcall (getf (getf c :handler) :update) midi-events))
	       ;; Update voices
	       (dolist (midi-event midi-events)
		 (if midi-event
		     (cond
		       ((cl-synthesizer-midi-event:note-on-eventp midi-event)
			(multiple-value-bind (voice-index voice-note stack-size)
			    (cl-synthesizer-midi-voice-manager:push-note
			     voice-manager
			     (cl-synthesizer-midi-event:get-note-number midi-event))
			  (let ((voice-state (elt voice-states voice-index)))
			    ;; if first note then set gate to on
			    (if (= 1 stack-size)
				(set-voice-state-gate voice-state cv-gate-on))
			    (set-voice-state-cv
			     voice-state
			     (funcall note-number-to-cv voice-note)))))
		       ((cl-synthesizer-midi-event:note-off-eventp midi-event)
			(multiple-value-bind (voice-index voice-note)
			    (cl-synthesizer-midi-voice-manager:remove-note
			     voice-manager
			     (cl-synthesizer-midi-event:get-note-number midi-event))
			  (if voice-index
			      (let ((voice-state (elt voice-states voice-index)))
				;; if no note left then set gate to off
				(if (not voice-note)
				    (set-voice-state-gate voice-state cv-gate-off)
				    (set-voice-state-cv
				     voice-state
				     (funcall note-number-to-cv voice-note))))))))))))))
