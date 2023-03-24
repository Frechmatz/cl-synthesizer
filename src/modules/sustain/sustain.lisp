(in-package :cl-synthesizer-modules-sustain)

(defun make-module (name environment
		    &key (trigger-threshold 2.5) (gate-threshold 2.5))
  "Creates a module which holds a given input as long as its gate input is \"on\".
    Main purpose of this module is to create envelope generators by chaining
    multiple ramp and sustain modules. <p>The function has the following parameters:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:trigger-threshold Minimum value of the :trigger input that indicates that the trigger is active.</li>
	<li>:gate-threshold Minimum value of the :gate input that indicates that the gate is on.</li>
    </ul></p>
    <p>The module has the following inputs:
    <ul>
	<li>:trigger Trigger input. If the trigger is active (see also :trigger-threshold), the module samples
	    its current input value and begins passing it to its output socket.</li>
	<li>:input Input value.</li>
	<li>:pass-through If value is >= 5.0 the module passes through its input value.</li>
	<li>:gate A gate signal (see also :gate-threshold).</li>
    </ul></p>
    <p>The module has the following outputs:
    <ul>
	<li>:output Output value of the module. The initial output value is 0.0.</li>
	<li>:busy A value >= 5.0 indicates that the module is busy by either passing through
	    its input value or holding the sampled input value until the gate input falls to zero.</li>
	<li>:done A trigger signal that jumps to 5.0 for the length of one clock cycle when the sustain cycle
	    has finished.</li>
	<li>:gate Passed through :gate input. Purpose of this output is to support more convenient
	    chaining of ramp and sustain modules.</li>
    </ul></p>
    This module has been inspired by <a href=\"https://github.com/dhemery/DHE-Modules/wiki/Multi-Stage-Envelopes\">dhemery</a>"
  (declare (ignore name environment))
  ;; (declare (optimize (debug 3) (speed 0) (space 0)))
  (let* ((output 0.0) (busy 0.0) (done 0.0) (passthrough-gate nil)
	 (input-trigger nil) (input-input nil) (input-pass-through nil) (input-gate nil))
    (let ((inputs (list
		   :trigger (list
			     :set (lambda(value) (setf input-trigger value))
			     :get (lambda() input-trigger))
		   :input (list
			   :set (lambda(value) (setf input-input value))
			   :get (lambda() input-input))
		   :pass-through (list
				  :set (lambda(value) (setf input-pass-through value))
				  :get (lambda() input-pass-through))
		   :gate (list
			  :set (lambda(value) (setf input-gate value))
			  :get (lambda() input-gate))))
	  (outputs (list
		    :output (list :get (lambda() output))
		    :busy (list :get (lambda() busy))
		    :done (list :get (lambda() done))
		    :gate (list :get (lambda() passthrough-gate)))))
    (list
     :inputs (lambda() inputs)
     :outputs (lambda() outputs)
     :update (lambda ()
	       (setf done 0.0)
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
			 (progn
			   (setf busy 5.0)
			   (setf output input-input)))
		     ;; Only continue when busy
		     (if (> busy 0.0)
			 (if (<= input-gate gate-threshold)
			     (progn
			       ;;(break)
			       (setf done 5.0)
			       (setf busy 0.0)))))))))))
