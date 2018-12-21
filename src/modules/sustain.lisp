(in-package :cl-synthesizer-modules-sustain)

(defun make-module (name environment
		    &key (trigger-threshold 2.5) (gate-threshold 2.5))
  "Creates a module which holds a given input as long as its gate input is \"on\".
    Main purpose of this module is to create envelope generators by chaining
    multiple ramp and sustain modules. The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:trigger-threshold Minimum value of the :trigger input that indicates that the trigger is active.</li>
	<li>:gate-threshold Minimum value of the :gate input that indicates that the gate is on.</li>
    </ul>
    The module has the following inputs:
    <ul>
	<li>:trigger Trigger input. If the trigger is active (see also :trigger-threshold), the module samples
	    its current input value and begins passing it to its output socket.</li>
	<li>:input Input value.</li>
	<li>:pass-through If value is >= 5.0 the module passes through its input value.</li>
	<li>:gate A gate signal (see also :gate-threshold).</li>
    </ul>
    The module has the following outputs:
    <ul>
	<li>:output Output value of the module. The initial output value is 0.0.</li>
	<li>:busy A value >= 5.0 indicates that the module is busy by either passing through
	    its input value or holding the sampled input value until the gate input falls to zero.</li>
	<li>:done A trigger signal that jumps to 5.0 for the length of one clock cycle when the sustain cycle
	    has finished.</li>
	<li>:gate Passed through :gate input. Purpose of this output is to support more convenient
	    chaining of ramp and sustain modules.</li>
    </ul>
    This module has been inspired by <a href=\"https://github.com/dhemery/DHE-Modules/wiki/Multi-Stage-Envelopes\">dhemery</a>"
  (declare (ignore environment))
  (declare (optimize (debug 3) (speed 0) (space 0)))
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
		     (setf busy 5.0)) ;; busy with passing through the input
		   (progn 
		     (if (>= trigger trigger-threshold)
			 (progn
			   (setf busy 5.0)
			   (setf output input)))
		     ;; Only continue when busy
		     (if (> busy 0.0)
			 (if (<= gate gate-threshold)
			     (progn
			       ;;(break)
			       (setf done 5.0)
			       (setf busy 0.0))))))))))
