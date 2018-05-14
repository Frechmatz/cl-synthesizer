;;
;;
;; ADSR Envelope Generator
;;
;;
;; Work in progress
;;
;;

(in-package :cl-synthesizer-modules-adsr)

;;
;; A dummy ADSR implementation
;;

(defun adsr (name environment &key (v-peak 5))
  (declare (ignore name))
  (let* ((sample-rate (getf environment :sample-rate))
	 (is-gate nil)
	 (cur-cv 0))
    (declare (ignore sample-rate))
    (list
     :inputs (lambda () '(:gate))
     :outputs (lambda () '(:cv))
     :get-output (lambda (output)
		   (declare (ignore output))
		   cur-cv)
     :update (lambda (&key (gate 0))
	       (setf is-gate (>= gate 4.9))
	       (setf cur-cv (if is-gate v-peak 0))))))


;;
;; New ADSR implementation
;;

(defun segments-controller (segments)
  "Manages a list of segments that define an envelope. A segment is defined 
as a plist with the following properties:
- :init -- A function that is called when the envelope enters the segment.
- :update -- A function that is called in order to update the segment (to process a tick).
The update function must return :DONE or :CONTINUE. In the case of :CONTINUE
the controller switches forward to the next segment (if available) and 
initializes and updates it.

Returns a function with the following arguments:
- restart -- If t then the controller switches to the first segment. Otherwise
it continues with the current segment or does nothing if there is no current segment.
The function returns the current segment index or nil."
  (let ((cur-segment-index nil)
	(segment-array
	 (make-array
	  (length segments)
	  :initial-contents segments)))
    (labels ((activate-segment (segment-index)
	       (declare (optimize (debug 3) (speed 0) (space 0)))
	       (format t "~%Activating segment ~a~%" segment-index)
	       (setf cur-segment-index segment-index)
	       (funcall (getf (elt segment-array cur-segment-index) :init)))
	     (activate-next-segment ()
	       (declare (optimize (debug 3) (speed 0) (space 0)))
	       (format t "~%Activate next segment~%")
	       (if (>= (+ 1 cur-segment-index) (length segments))
		   (progn
		     (setf cur-segment-index nil)
		     nil)
		   (progn
		     ;;(break)
		     (activate-segment (+ 1 cur-segment-index))
		     t)))
	     (update-segment (restart)
	       (declare (optimize (debug 3) (speed 0) (space 0)))
	       (if restart
		   (progn
		     (format t "~%Activating first segment~%")
		     (activate-segment 0))
		   (if cur-segment-index
		       (let ((segment-state (funcall (getf (elt segment-array cur-segment-index) :update))))
			 (if (and (eq :CONTINUE segment-state) (activate-next-segment))
			     (update-segment nil)))))))
      (lambda (restart)
	(update-segment restart)
	cur-segment-index))))
    

;; TODO Find better keyword for decay target voltage :decay-v
(defun adsr2 (name environment &key (v-peak 5) (attack-ms 1000) (decay-ms 1000) (decay-v 3) (release-ms 1000))  
  (declare (ignore name ))
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let* ((sample-rate (getf environment :sample-rate))
	 (is-gate nil)
	 (cur-cv 0)
	 ;; error is about 2 promille at sample frequency of 44100 
	 (ticks-per-ms (floor (/ sample-rate 1000))))
    (labels ((has-segment-completed (elapsed-ticks total-ticks requires-gate)
	       (declare (optimize (debug 3) (speed 0) (space 0)))
	       (if (and requires-gate (not is-gate))
		   t
		   (if (and elapsed-ticks total-ticks (>= elapsed-ticks total-ticks))
		       t
		       nil))))
      (let ((controller (segments-controller
		   (list
		    ;; a
		    (let ((total-ticks nil) (elapsed-ticks nil) (transfer-fn nil))
		      (list
		       :init (lambda ()
			       (setf total-ticks (* ticks-per-ms attack-ms))
			       (setf elapsed-ticks -1)
			       (setf transfer-fn (cl-synthesizer-core:linear-converter
						  :input-min 0
						  :input-max total-ticks
						  :output-min 0
						  :output-max 1.0)))
		       :update (lambda()
			       (setf elapsed-ticks (+ 1 elapsed-ticks))
			       (if (has-segment-completed elapsed-ticks total-ticks t)
				   (progn
				     ;;(break)
				     :CONTINUE)
				   (progn
				     (setf cur-cv (funcall (getf transfer-fn :get-y) elapsed-ticks))
				     :DONE)))))
		    ;; d
		    (let ((total-ticks nil) (elapsed-ticks nil) (transfer-fn nil))
		      (list
		       :init (lambda ()
			       (setf total-ticks (* ticks-per-ms decay-ms))
			       (setf elapsed-ticks -1)
			       (setf transfer-fn (cl-synthesizer-core:linear-converter
						  :input-min 0
						  :input-max total-ticks
						  :output-min cur-cv
						  :output-max (/ decay-v v-peak))))
		       :update (lambda()
			       (setf elapsed-ticks (+ 1 elapsed-ticks))
			       (if (has-segment-completed elapsed-ticks total-ticks t)
				   (progn
				     ;;(break)
				     :CONTINUE)
				   (progn
				     (setf cur-cv (funcall (getf transfer-fn :get-y) elapsed-ticks))
				     :DONE)))))
		    ;; s
		    (list
		     :init (lambda ())
		     :update (lambda()
			     (if (has-segment-completed nil nil t)
				 :CONTINUE
				 :DONE)))
		    ;; r
		    (let ((total-ticks nil) (elapsed-ticks nil) (transfer-fn nil))
		      (list
		       :init (lambda ()
			       (setf total-ticks (* ticks-per-ms release-ms))
			       (setf elapsed-ticks -1)
			       (setf transfer-fn (cl-synthesizer-core:linear-converter
						  :input-min 0
						  :input-max total-ticks
						  :output-min cur-cv
						  :output-max 0)))
		       :update (lambda()
			       (setf elapsed-ticks (+ 1 elapsed-ticks))
			       (if (has-segment-completed elapsed-ticks total-ticks nil)
				   (progn
				     ;;(break)
				     :CONTINUE)
				   (progn
				     (setf cur-cv (funcall (getf transfer-fn :get-y) elapsed-ticks))
				     :DONE)))))
		    ))))
	(list
	 :inputs (lambda () '(:gate))
	 :outputs (lambda () '(:cv))
	 :get-output (lambda (output)
		       (declare (ignore output))
		       (* v-peak cur-cv))
	 :update (lambda (&key (gate 0))
		   (declare (optimize (debug 3) (speed 0) (space 0)))
		   (let ((previous-gate is-gate) (restart nil))
		     (setf is-gate (if (>= gate 4.9) t nil))
		     (setf restart (and is-gate (not previous-gate)))
		     (if restart
			 (setf cur-cv 0))
		     (funcall controller restart))))))))

