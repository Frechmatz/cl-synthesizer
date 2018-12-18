;;
;;
;;

(in-package :cl-synthesizer-modules-ramp)

(defun make-module (name environment
		    &key time-ms target-output (gate-state nil)
		      (trigger-threshold 2.5) (gate-threshold 2.5))
  "TODO
   It cannot be guaranteed that target-output will be exactly reached. 
   Due to the increments calculated out of time-ms and sample-rate 
   the ramp may stop at an output value a little bit smaller or 
   greater than the desired target-output value.
   TODO trigger has higher priority than pass-through"
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (if (and gate-state (not (eq gate-state :on)) (not (eq gate-state :off)))
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: Invalid gate-state ~a Must be one of nil, :on, :off"
       :format-arguments (list name gate-state)))
  (let* ((output 0.0) (busy 0.0) (done 0.0) (elapsed-time-ms 0.0)
	 (start 0.0) (passthrough-gate nil)
	 (sample-rate (getf environment :sample-rate))
	 (tick-delta-ms (/ 1 (/ sample-rate 1000.0))))
    (list
     :inputs (lambda () '(:trigger :input :pass-through :gate))
     ;; :busy -> Gate
     ;; :done -> Trigger
     ;; we need both :busy and :done due to retriggering.
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
	       (declare (optimize (debug 3) (speed 0) (space 0)))
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
			 ;; Start ramp
			 (progn
			   (setf busy 5.0)
			   (setf start input) ;; sample
			   (setf output input)
			   (setf elapsed-time-ms 0.0)))
		     ;; Only continue when busy
		     (if (> busy 0.0)
			 (if (or (<= time-ms 0.0)
				 (and (eq gate-state :on) (<= gate gate-threshold))
				 (and (eq gate-state :off) (> gate gate-threshold)))
			     (progn
			       ;;(break)
			       (setf done 5.0)
			       (setf busy 0.0))
			     (progn
			       (setf elapsed-time-ms (+ elapsed-time-ms tick-delta-ms))
			       (if (> (cl-synthesizer-core:round-time elapsed-time-ms sample-rate) time-ms)
				   (progn
				     (setf done 5.0)
				     (setf busy 0.0))
				   (let* ((ticks-per-cycle (* time-ms (/ sample-rate 1000)))
					  (delta (/ (- target-output start) ticks-per-cycle)))
				     (setf output (+ output delta)))))))))))))



