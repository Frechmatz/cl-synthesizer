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

(defun midi-interface (name environment &key
					  (voice-count 1)
					  (note-number-to-cv (lambda (note-number) (/ note-number 12)))
					  (play-mode :PLAY-MODE-POLY)
					  (controller-handler nil))
  "play-mode: :PLAY-MODE-POLY, :PLAY-MODE-UNISONO
   controller-handler: A list of controller handlers. Each entry consists of a list of
   <output-keyword> (list :update lambda (midi-events) () :get-output :lambda ()())"
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let* ((voice-states (make-array voice-count))
	 (output-socket-lookup-table (make-hash-table :test #'eq))
	 (voice-manager (make-instance 'cl-synthesizer-midi-voice-manager:voice-manager
				       :voice-count (if (eq play-mode :PLAY-MODE-POLY) voice-count 1)))
	 (outputs (concatenate 'list
			       (cl-synthesizer-macro-util:make-keyword-list "CV" voice-count)
			       (cl-synthesizer-macro-util:make-keyword-list "GATE" voice-count))))
    ;; append controller handlers to outputs
    (setf outputs (concatenate 'list
			       outputs
			       (mapcar (lambda (i)
					 (let ((output-keyword (first i)))
					   (if (not (keywordp output-keyword))
					       (cl-synthesizer:signal-assembly-error
						:format-control "Module ~a: Controller handler output identifier ~a must be a keyword"
						:format-arguments (list name output-keyword)))
					   (if (find output-keyword outputs)
					       (cl-synthesizer:signal-assembly-error
						:format-control "Module ~a: Controller handler output identifier ~a is not available"
						:format-arguments (list name output-keyword)))
					   output-keyword))
				       controller-handler)))
    (dotimes (i voice-count)
      (if (or (= 0 i) (not (eq play-mode :PLAY-MODE-UNISONO)))
	  (setf (elt voice-states i) (make-voice-state name environment i))
	  (setf (elt voice-states i) (elt voice-states 0)))
      (let ((cur-i i)) ;; new context for the lambdas
	(setf (gethash (cl-synthesizer-macro-util:make-keyword "CV" cur-i) output-socket-lookup-table)
	      (lambda () (elt (elt voice-states cur-i) +voice-state-cv+)))
	(setf (gethash (cl-synthesizer-macro-util:make-keyword "GATE" cur-i) output-socket-lookup-table)
	      (lambda () (elt (elt voice-states cur-i) +voice-state-gate+)))))
    ;; add controller handlers to lookup table
    (dolist (cc-handler controller-handler)
      (let ((cur-cc-handler cc-handler)) ;; new context
	  (if (or (not (listp (second cur-cc-handler))) (= 0 (length (second cur-cc-handler))))
	      (cl-synthesizer:signal-assembly-error
	       :format-control "Module ~a: Controller handler object ~a must be a non-empty list"
	       :format-arguments (list name (first cur-cc-handler))))
	  (if (not (getf (second cur-cc-handler) :get-output))
	      (cl-synthesizer:signal-assembly-error
	       :format-control "Module ~a: Controller handler object ~a must provide a 'get-output' function property"
	       :format-arguments (list name (first cur-cc-handler))))
	  (if (not (getf (second cur-cc-handler) :update))
	      (cl-synthesizer:signal-assembly-error
	       :format-control "Module ~a: Controller handler object ~a must provide a 'update' function property"
	       :format-arguments (list name (first cur-cc-handler))))
	  (setf (gethash (first cur-cc-handler) output-socket-lookup-table)
		(lambda () (funcall (getf (second cur-cc-handler) :get-output))))))
    (list
     :inputs (lambda () '(:midi-events))
     :outputs (lambda () outputs)
     :get-output (lambda (output)
		   (let ((handler (gethash output output-socket-lookup-table)))
		     (if (not handler)
			 (error (format nil "Unknown input ~a requested from ~a" output name)))
		     (funcall handler)))
     :update (lambda (&key (midi-events nil))
	       ;;(declare (optimize (debug 3) (speed 0) (space 0)))
	       ;;(break)
	       ;; Update controllers
	       (dolist (c controller-handler)
		 (funcall (getf (second c) :update) midi-events))
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
			    (if (= 1 stack-size)
				(progn
				  (setf (elt voice-state +voice-state-gate+) 5.0)))
			    (setf (elt voice-state +voice-state-cv+) (funcall note-number-to-cv voice-note))
			    ;;(format t "cv-oct: ~a~%" (elt voice-state +voice-state-cv+))
			    )))
		       ((cl-synthesizer-midi-event:note-off-eventp midi-event)
			(multiple-value-bind (voice-index voice-note)
			    (cl-synthesizer-midi-voice-manager:remove-note
			     voice-manager
			     (cl-synthesizer-midi-event:get-note-number midi-event))
			  (if voice-index
			      (let ((voice-state (elt voice-states voice-index)))
				(if (not voice-note)
				    (progn
				      (setf (elt voice-state +voice-state-gate+) 0))
				    (progn
				      (setf (elt voice-state +voice-state-cv+) (funcall note-number-to-cv voice-note))
				      ))
				;;(format t "cv-oct: ~a~%" (elt voice-state +voice-state-cv+))
				))))
		       )))))))
