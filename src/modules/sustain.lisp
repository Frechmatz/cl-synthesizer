(in-package :cl-synthesizer-modules-sustain)

(defun make-module (name environment
		    &key (trigger-threshold 2.5) (gate-threshold 2.5))
  ""
  (declare (ignore environment))
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let* ((output 0.0) (busy 0.0) (done 0.0) (passthrough-gate nil))
    (list
     :inputs (lambda () '(:trigger :input :pass-through :gate))
     :outputs (lambda () '(:output :busy :done :gate))
     :get-output (lambda (output-socket)
		   (cond
		     ((eq :output output-socket)
		      output)
		     ((eq :busy output-socket)
		      busy)
		     ((eq :done output-socket)
		      done)
		     ((eq :gate output-socket)
		      passthrough-gate)
		     (t
		      (error (format nil "Output socket ~a not supported by module ~a" output-socket name)))))
     :update (lambda (&key trigger input pass-through gate)
	       ;;(declare (optimize (debug 3) (speed 0) (space 0)))
	       (setf done 0.0)
	       (setf passthrough-gate gate)
	       (if (not gate)
		   (setf gate 0.0))
	       (if (not input)
		   (setf input 0.0))
	       (if (not trigger)
		   (setf trigger 0.0))
	       (if (not pass-through)
		   (setf pass-through 0.0))
	       (if (> pass-through 0.0)
		   (progn 
		     (setf output input)
		     (setf busy 5.0)) ;; busy with deferring the input
		   (progn 
		     (if (>= trigger trigger-threshold)
			 (progn
			   (setf busy 5.0)
			   (setf output input)))
		     ;; Only continue when busy
		     (if (> busy 0.0)
			 (if (<= gate gate-threshold)
			     (progn
			       (setf done 5.0)
			       (setf busy 0.0))))))))))
