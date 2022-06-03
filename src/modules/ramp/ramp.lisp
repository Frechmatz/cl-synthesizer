(in-package :cl-synthesizer-modules-ramp)


(defun round-time (time sample-rate)
  "Rounds a time value by factoring in the time resolution of the sample rate.</br>
   Examples:</br>
   (round-time 10.000005 44100) => 10.0</br>
   (round-time 10.00005 44100)  => 10.0000454</br>
   (round-time -10.000005 44100) => -10.0</br>
   (round-time -10.00005 44100) => -10.0000454"
  (declare (type single-float time sample-rate))
  (/ (round time (/ 1.0 sample-rate)) sample-rate))

(defun make-module (name environment
		    &key time-ms
		      target-output
		      (gate-state nil)
		      (trigger-threshold 2.5)
		      (gate-threshold 2.5)
		      (exponential nil))
  "Creates a module whose output climbs from a given input value to a given output value
    in a given time. Main purpose of this module is to create envelope generators by chaining
    multiple ramp and sustain modules. <p>The function has the following parameters:
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
        <li>:exponential If t then the ramp will climb with an exponential characteristic.</li>
    </ul></p>
    <p>The module has the following inputs:
    <ul>
	<li>:trigger Trigger input. If the trigger is active (see also :trigger-threshold), the module samples
	    its current input value and begins climbing to the desired target output value.</li>
	<li>:input Input value.</li>
	<li>:pass-through If value is >= 5.0 the module passes through its input value.</li>
	<li>:gate A gate signal (see also :gate-threshold).</li>
	<li>:cv-time NIL or climbing time (duration) of the ramp. The resulting time is 1000ms per Volt.</li>
    </ul></p>
    <p>The module has the following outputs:
    <ul>
	<li>:output Output value of the module. The initial output value is 0.0.</li>
	<li>:busy A value >= 5.0 indicates that the module is busy by either passing through
	its input value or climbing to the target output value.</li>
	<li>:done A trigger signal that jumps to 5.0 for the length of one clock cycle when the ramp has
	finished.</li>
	<li>:gate Passed through :gate input. Purpose of this output is to support more convenient
	    chaining of ramp and sustain modules.</li>
    </ul></p>
    When the ramp aborts due to a toggling Gate signal or when its supposed
    duration has been exceeded due to time modulation then the output value does not jump 
    to the desired target-output but stays at its current value.<br><br>
    This module has been inspired by <a href=\"https://github.com/dhemery/DHE-Modules/wiki/Multi-Stage-Envelopes\">dhemery</a>"
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (if (and gate-state (not (eq gate-state :on)) (not (eq gate-state :off)))
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: Invalid gate-state '~a' Must be one of nil, :on, :off"
       :format-arguments (list name gate-state)))
  (let* ((time-cv-to-time-ms (lambda(time-cv) (* (abs time-cv) 1000)))
	 (output 0.0) (busy 0.0) (done 0.0) (elapsed-time-ms 0.0)
	 (start 0.0) (passthrough-gate nil)
	 (sample-rate (getf environment :sample-rate))
	 (tick-delta-ms (/ 1 (/ sample-rate 1000.0)))
	 (input-trigger nil) (input-input nil) (input-pass-through nil) (input-gate nil)
	 (input-cv-time nil)
	 (transfer-fn nil))
    (if (not exponential)
	;; Linear transfer
	(setf transfer-fn (lambda()
			    (let* ((ticks-per-cycle (* time-ms (/ sample-rate 1000)))
				   (delta (/ (- target-output start) ticks-per-cycle)))
			      (+ output delta))))
	;; Exponential transfer
	(setf transfer-fn (lambda()
			    (let* ((ramp-progress-percent (/ elapsed-time-ms time-ms))
				   (normalized-delta (cl-synthesizer-core:normalized-exp ramp-progress-percent)))
			      (+ start (* (- target-output start) normalized-delta))))))
    (let ((inputs (list
		   :trigger (lambda(value) (setf input-trigger value))
		   :input (lambda(value) (setf input-input value))
		   :pass-through (lambda(value) (setf input-pass-through value))
		   :gate (lambda(value) (setf input-gate value))
		   :cv-time (lambda(value) (setf input-cv-time  value))))
	  (outputs (list
		    :output (lambda() output)
		    :busy (lambda() busy)
		    :done (lambda() done)
		    :gate (lambda() passthrough-gate))))
      (list
       :inputs (lambda () inputs)
       :outputs (lambda () outputs)
       :update (lambda ()
		 (setf done 0.0)
		 (if input-cv-time
		     (setf time-ms (funcall time-cv-to-time-ms input-cv-time)))
		 (setf passthrough-gate input-gate)
		 (if (not input-gate)
		     (setf input-gate 0.0))
		 (if (not input-input)
		     (setf input-input 0.0))
		 (if (not input-trigger)
		     (setf input-trigger 0.0))
		 (if (not input-pass-through)
		     (setf input-pass-through 0.0))
		 (if (> input-pass-through 0.0)
		     (progn
		       (setf output input-input)
		       (setf busy 5.0)) ;; busy with passing through the input
		     (progn 
		       (if (>= input-trigger trigger-threshold)
			   ;; Start ramp
			   (progn
			     (setf busy 5.0)
			     (setf start input-input) ;; sample
			     (setf output input-input)
			     (setf elapsed-time-ms 0.0)))
		       ;; Only continue when busy
		       (if (> busy 0.0)
			   (if (or (<= time-ms 0.0)
				   (and (eq gate-state :on) (<= input-gate gate-threshold))
				   (and (eq gate-state :off) (> input-gate gate-threshold)))
			       (progn
				 (setf done 5.0)
				 (setf busy 0.0))
			       (progn
				 (setf elapsed-time-ms (+ elapsed-time-ms tick-delta-ms))
				 (if (> (round-time elapsed-time-ms sample-rate) time-ms)
				     (progn
				       (setf done 5.0)
				       (setf busy 0.0))
				     (setf output (funcall transfer-fn)))))))))))))
