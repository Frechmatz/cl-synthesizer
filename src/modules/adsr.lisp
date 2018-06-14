;;
;;
;; ADSR Envelope Generator
;;
;;
;; Work in progress
;;
;;

(in-package :cl-synthesizer-modules-adsr)

(defun segments-controller (segments)
  "Manages a list of segments that define an envelope. A segment is defined 
as a plist with the following properties:
- :init -- A function that is called when the envelope enters the segment.
- :update -- A function that is called in order to update the segment (to process a tick).
The update function must return :DONE or :CONTINUE. In the case of :CONTINUE
the controller switches forward to the next segment (if available) and 
initializes and updates it.
Returns a function with the following arguments:
- restart -- If t the controller switches to the first segment. Otherwise
it continues with the current segment or does nothing if there is no current segment.
The function returns the current segment index or nil."
  (let ((cur-segment-index nil)
	(segment-array
	 (make-array
	  (length segments)
	  :initial-contents segments)))
    (labels ((activate-segment (segment-index)
	       (declare (optimize (debug 3) (speed 0) (space 0)))
	       (setf cur-segment-index segment-index)
	       (funcall (getf (elt segment-array cur-segment-index) :init)))
	     (activate-next-segment ()
	       (declare (optimize (debug 3) (speed 0) (space 0)))
	       (if (>= (+ 1 cur-segment-index) (length segments))
		   (progn
		     (setf cur-segment-index nil)
		     nil)
		   (progn
		     (activate-segment (+ 1 cur-segment-index))
		     t)))
	     (update-segment (restart)
	       (declare (optimize (debug 3) (speed 0) (space 0)))
	       (if restart
		   (activate-segment 0)
		   (if cur-segment-index
		       (let ((segment-state (funcall (getf (elt segment-array cur-segment-index) :update))))
			 (if (and (eq :CONTINUE segment-state) (activate-next-segment))
			     (update-segment nil)))))))
      (lambda (restart)
	(update-segment restart)
	cur-segment-index))))
    

;;
;; Segment macros (to be used within the ADSR module function definition)
;;

(defmacro hold ((&key segment-name))
  "Generates a segment whose result remains at cur-cv until the gate drops to 0."
  `(flet ((has-segment-completed ()
	    (not is-gate)))
     (list
      :init (lambda ()
	      (format t "~%Initializing segment ~a~%" ,segment-name))
      :update (lambda()
		(if (has-segment-completed)
		    :CONTINUE
		    :DONE)))))

(defmacro ramp ((&key segment-name requires-gate target-cv time-ms))
  "Generates a segment whose output starts at cur-cv and descends/ascends to the given target
voltage within the given time interval. Depending on the 'requires-gate' parameter the segment
terminates when the gate drops to 0."
  (declare (ignore segment-name))
  ;;(format t "~%RAMP Macro called~%")
  `(let ((total-ticks nil) (elapsed-ticks nil) (transfer-fn nil))
     (flet ((has-segment-completed ()
	      (if (and ,requires-gate (not is-gate))
		  t
		  (and elapsed-ticks total-ticks (>= elapsed-ticks total-ticks)))))
       (list
	:init (lambda ()
		(setf total-ticks (* ticks-per-ms ,time-ms))
		(setf elapsed-ticks -1)
		(setf transfer-fn (cl-synthesizer-core:linear-converter
				   :input-min 0
				   :input-max total-ticks
				   :output-min cur-cv
				   :output-max ,target-cv)))
	:update (lambda()
		  (setf elapsed-ticks (+ 1 elapsed-ticks))
		  (if (has-segment-completed)
		      :CONTINUE
		      (progn
			(setf cur-cv (funcall (getf transfer-fn :get-y) elapsed-ticks))
			:DONE)))))))
  
(defun adsr (name environment &key (attack-ms 1000) (attack-cv 5) (decay-ms 1000) (decay-cv 3) (release-ms 1000))  
  (declare (ignore name))
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let* ((is-gate nil)
	 (cur-cv 0)
	 (ticks-per-ms (floor (/ (getf environment :sample-rate) 1000)))
	 (controller
	  (segments-controller
	   (list
	    (ramp (:segment-name "Attack" :requires-gate t :target-cv attack-cv :time-ms attack-ms))
	    (ramp (:segment-name "Decay" :requires-gate t :target-cv decay-cv :time-ms decay-ms))
	    (hold (:segment-name "Sustain"))
	    (ramp (:segment-name "Release" :requires-gate nil :target-cv 0 :time-ms release-ms))))))
    (list
     :inputs (lambda () '(:gate))
     :outputs (lambda () '(:cv))
     :get-output (lambda (output)
		   (declare (ignore output))
		   cur-cv)
     :update (lambda (&key (gate 0))
	       ;;(declare (optimize (debug 3) (speed 0) (space 0)))
	       (let ((previous-gate is-gate) (restart nil))
		 (setf is-gate (if (>= gate 4.9) t nil))
		 (setf restart (and is-gate (not previous-gate)))
		 (if restart
		     (setf cur-cv 0))
		 (funcall controller restart))))))

(defmacro envelope (segments)
  (let ((segment-def nil))
    ;; compile segments
    (dolist (segment segments)
      (cond
	((eq :ramp (getf segment :type))
	 (push `(ramp
		 (:segment-name
		  ,(getf segment :segment-name)
		  :requires-gate
		  ,(getf segment :requires-gate)
		  :target-cv
		  ,(getf segment :target-cv)
		  :time-ms
		  ,(getf segment :time-ms)))
	       segment-def))
	((eq :hold (getf segment :type))
	 (push `(hold
		 (:segment-name
		  ,(getf segment :segment-name)))
	       segment-def))
	(t (cl-synthesizer:signal-assembly-error
	    :format-control "Unsupported segment type: ~a"
	    :format-arguments (list (getf segment :type))))))
    (setf segment-def (reverse segment-def))
    ;;(format t "~%Compiled segments: ~a~%" segment-def)
    `(lambda(name environment)
       (declare (ignore name))
       (let* ((is-gate nil)
	      (cur-cv 0)
	      (ticks-per-ms (floor (/ (getf environment :sample-rate) 1000)))
	      (controller (segments-controller (list ,@segment-def))))
	 (list
	  :inputs (lambda () '(:gate))
	  :outputs (lambda () '(:cv))
	  :get-output (lambda (output)
			(declare (ignore output))
			cur-cv)
	  :update (lambda (&key (gate 0))
		    (let ((previous-gate is-gate) (restart nil))
		      (setf is-gate (if (>= gate 4.9) t nil))
		      (setf restart (and is-gate (not previous-gate)))
		      (if restart
			  (setf cur-cv 0))
		      (funcall controller restart))))))))

