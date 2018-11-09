(in-package :cl-synthesizer-modules-midi-cc-interface)

(defun midi-cc-interface (name environment
			  &key controller-numbers
			    transform-handler
			    (channel nil)
			    (initial-output 0)
			    (min-output nil)
			    (max-output nil))
  "Creates a MIDI CC Event interface module. The module maps MIDI control change events to
   an output value. The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:controller-numbers A list of MIDI controller numbers.</li>
        <li>:transform-handler A function that converts a control value to 
            the output value of the module. It is called for each 
            matching CC event and has the following arguments:
            <ul>
                <li>The current output value of the module.</li>
                <li>Controller number.</li>
                <li>Control value.</li>
            </ul>
            The function must return the new output value of the module.
        </li>
        <li>:channel Optional number of the MIDI channel to which the controller events 
          must belong. By default there is no channel filtering applied.</li>
        <li>:initial-output The initial output value of the module.</li>
        <li>:min-output Optional lowest numeric output value of the module. If
        the transform handler returns a number smaller than min-output then
        the actual output-value is set to min-output.</li>
        <li>:max-output Optional largest numeric output value of the module. If
          the transform handler returns a number greater than max-output then
          the actual output value is set to max-output.</li>
    </ul>
    The module has the following inputs:
    <ul>
	<li>:midi-events A list of MIDI events.</li>
    </ul>
    The module has the following outputs:
    <ul>
	<li>:output The current output value.</li>
    </ul>"
  (declare (ignore environment))
  (if (not controller-numbers)
      (cl-synthesizer:signal-assembly-error
       :format-control "controller-numbers must not be nil ~a"
       :format-arguments (list name)))
  (if (not (listp controller-numbers))
      (cl-synthesizer:signal-assembly-error
       :format-control "controller-numbers must be a list ~a"
       :format-arguments (list name)))
  (if (not (functionp transform-handler))
      (cl-synthesizer:signal-assembly-error
       :format-control "transform-handler must be a function ~a"
       :format-arguments (list name)))
  (flet ((clip (v)
	   (cond
	     ((not v) v)
	     ((and min-output (> min-output v)) min-output)
	     ((and max-output (< max-output v)) max-output)
	     (t v))))
    (let ((cur-output (clip initial-output)))
      (list
       :inputs (lambda () '(:midi-events))
       :outputs (lambda () '(:output))
       :get-output (lambda (output)
		     (declare (ignore output))
		     cur-output)
       :update (lambda (&key midi-events)
		 (dolist (midi-event midi-events)
		   (if (and midi-event
			    (cl-synthesizer-midi-event:control-change-eventp midi-event)
			    (find (cl-synthesizer-midi-event:get-controller-number midi-event) controller-numbers)
			    (or (not channel)
				(= channel (cl-synthesizer-midi-event:get-channel midi-event))))
		       (setf cur-output
			     (clip (funcall
				    transform-handler 
				    cur-output
				    (cl-synthesizer-midi-event:get-controller-number midi-event)
				    (cl-synthesizer-midi-event:get-controller-value midi-event)))))))))))


