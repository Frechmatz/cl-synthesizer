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

;; TODO Find better keyword for decay target voltage :decay-v
(defun adsr2 (name environment &key (v-peak 5) (attack-ms 1000) (decay-ms 1000) (decay-v 3) (release-ms 1000))  
  (declare (ignore name decay-ms decay-v))
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let* ((sample-rate (getf environment :sample-rate))
	 (is-gate nil)
	 (cur-cv 0)
	 (cur-segment-index nil)
	 ;; error is about 2 promille at sample frequency of 44100 
	 (ticks-per-ms (floor (/ sample-rate 1000))))
    (labels ((has-segment-completed (elapsed-ticks total-ticks requires-gate)
	       (declare (optimize (debug 3) (speed 0) (space 0)))
	       (if (and requires-gate (not is-gate))
		   t
		   (if (and elapsed-ticks total-ticks (>= elapsed-ticks total-ticks))
		       t
		       nil))))
      (let ((segments
	     (make-array
	      2
	      :initial-contents
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
		  :tick (lambda()
			  (setf elapsed-ticks (+ 1 elapsed-ticks))
			  (if (has-segment-completed elapsed-ticks total-ticks t)
			      (progn
				;;(break)
				:CONTINUE)
			      (progn
				(setf cur-cv (* v-peak (funcall (getf transfer-fn :get-y) elapsed-ticks)))
				:DONE)))))
	       ;; r
	       (let ((total-ticks nil) (elapsed-ticks nil))
		 (list
		  :init (lambda ()
			  (setf total-ticks (* ticks-per-ms release-ms))
			  (setf elapsed-ticks -1))
		  :tick (lambda()
			  (setf elapsed-ticks (+ 1 elapsed-ticks))
			  (if (has-segment-completed elapsed-ticks total-ticks nil)
			      :CONTINUE
			      (progn
				(setf cur-cv (* 0.5 v-peak))
				:DONE)))))
	       ))))
	(labels ((activate-segment (segment-index)
		   (declare (optimize (debug 3) (speed 0) (space 0)))
		   (format t "~%Activating segment ~a~%" segment-index)
		   (setf cur-segment-index segment-index)
		   (funcall (getf (elt segments cur-segment-index) :init)))
		 (activate-next-segment ()
		   (declare (optimize (debug 3) (speed 0) (space 0)))
		   (if (>= (+ 1 cur-segment-index) (length segments))
		       (progn
			 (setf cur-cv 0.0) ;; no more segment, for now set cv to 0
			 nil)
		       (progn
			 ;;(break)
			 (activate-segment (+ 1 cur-segment-index))
			 t)))
		 (handle-segment (restart-envelope)
		   (declare (optimize (debug 3) (speed 0) (space 0)))
		   (if restart-envelope
		       (progn
			 (format t "~%Restarting envelope~%")
			 (setf cur-cv 0.0)
			 (activate-segment 0))
		       (if cur-segment-index
			   (let ((segment-state (funcall (getf (elt segments cur-segment-index) :tick))))
			     (if (and (eq :CONTINUE segment-state) (activate-next-segment))
				 (handle-segment nil)))))))
	  (list
	   :inputs (lambda () '(:gate))
	   :outputs (lambda () '(:cv))
	   :get-output (lambda (output)
			 (declare (ignore output))
			 cur-cv)
	   :update (lambda (&key (gate 0))
		     (declare (optimize (debug 3) (speed 0) (space 0)))
		     (let ((previous-gate is-gate))
		       (setf is-gate (if (>= gate 4.9) t nil))
		       (if (and is-gate (not previous-gate))
			   (handle-segment t)
			   (handle-segment nil))))))))))
