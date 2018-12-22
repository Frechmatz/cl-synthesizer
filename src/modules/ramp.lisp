(in-package :cl-synthesizer-modules-ramp)

(defun make-module (name environment
		    &key time-ms target-output (gate-state nil)
		      (trigger-threshold 2.5) (gate-threshold 2.5)
		      (time-cv-to-time-ms nil))
  "Creates a module whose output climbs from a given input value to a given output value
    in a given time. Main purpose of this module is to create envelope generators by chaining
    multiple ramp and sustain modules. The module climbs linearly (exponential climbing will
    be added later). The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:time-ms Default climbing time (duration) in milliseconds.</li>
	<li>:target-output Desired target output value. Due to the time resolution given by the sample-rate 
	    of the environment the ramp may stop at an output value a little bit smaller or 
	    greater than the desired target-output value.</li>
	<li>:gate-state Required state of the Gate input. One of :on, :off, nil</li>
	<li>:trigger-threshold Minimum value of the :trigger input that indicates that the trigger is active.</li>
	<li>:gate-threshold Minimum value of the :gate input that indicates that the gate is on.</li>
	<li>:time-cv-to-time-ms An optional function that converts a time control voltage to a duration in milliseconds.
        The default implementation is 1000ms/1V (abs(cv-time) * 1000).</li>
    </ul>
    The module has the following inputs:
    <ul>
	<li>:trigger Trigger input. If the trigger is active (see also :trigger-threshold), the module samples
	    its current input value and begins climbing to the desired target output value.</li>
	<li>:input Input value.</li>
	<li>:pass-through If value is >= 5.0 the module passes through its input value.</li>
	<li>:gate A gate signal (see also :gate-threshold).</li>
	<li>:cv-time NIL or climbing time (duration) of the ramp (see also :time-cv-to-time-ms).</li>
    </ul>
    The module has the following outputs:
    <ul>
	<li>:output Output value of the module. The initial output value is 0.0.</li>
	<li>:busy A value >= 5.0 indicates that the module is busy by either passing through
	its input value or climbing to the target output value.</li>
	<li>:done A trigger signal that jumps to 5.0 for the length of one clock cycle when the ramp has
	finished.</li>
	<li>:gate Passed through :gate input. Purpose of this output is to support more convenient
	    chaining of ramp and sustain modules.</li>
    </ul>
    When the ramp aborts due to a toggling Gate signal or when its supposed
    duration has been exceeded due to time modulation then the output value does not jump 
    to the desired target-output but stays at its current value.<br><br>
    This module has been inspired by <a href=\"https://github.com/dhemery/DHE-Modules/wiki/Multi-Stage-Envelopes\">dhemery</a>"
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (if (and gate-state (not (eq gate-state :on)) (not (eq gate-state :off)))
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: Invalid gate-state ~a Must be one of nil, :on, :off"
       :format-arguments (list name gate-state)))
  (if (not time-cv-to-time-ms)
      (setf time-cv-to-time-ms (lambda(time-cv) (* (abs time-cv) 1000))))
  (let* ((output 0.0) (busy 0.0) (done 0.0) (elapsed-time-ms 0.0)
	 (start 0.0) (passthrough-gate nil)
	 (sample-rate (getf environment :sample-rate))
	 (tick-delta-ms (/ 1 (/ sample-rate 1000.0))))
    (list
     :inputs (lambda () '(:trigger :input :pass-through :gate :cv-time))
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
     :update (lambda (input-args
		      ;;&key trigger input pass-through gate cv-time
			  )
	       (let ((trigger (getf input-args :trigger))
		     (input (getf input-args :input))
		     (pass-through (getf input-args :pass-through))
		     (gate (getf input-args :gate))
		     (cv-time (getf input-args :cv-time)))
	       ;;(declare (optimize (debug 3) (speed 0) (space 0)))
	       (setf done 0.0)
	       (if cv-time
		   (setf time-ms (funcall time-cv-to-time-ms cv-time)))
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
		     (setf busy 5.0)) ;; busy with passing through the input
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
				     (setf output (+ output delta))))))))))))))
