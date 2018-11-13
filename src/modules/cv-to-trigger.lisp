(in-package :cl-synthesizer-modules-cv-to-trigger)

(defun make-module (name environment &key switching-voltage trigger-voltage)
  "Creates a Voltage to Trigger Converter module. 
   The module fires a one clock cycle long pulse when input-voltage >= switching-voltage,
   then stops firing until the input-voltage went below the switching-voltage.
   The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:switching-voltage The minimum value of the input voltage in order to fire.</li>
	<li>:trigger-voltage The voltage of the trigger signal.</li>
    </ul>
    The module has the following inputs:
    <ul>
	<li>:input The input voltage.</li>
    </ul>
    The module has the following outputs:
    <ul>
        <li>:output The output signal.</li>
    </ul>"
  (declare (ignore environment))
  (let ((cur-output 0) (is-wait nil))
    (list
     :inputs (lambda () '(:input))
     :outputs (lambda () '(:output))
     :get-output (lambda (output) (declare (ignore output)) cur-output)
     :update (lambda (&key input)
	       (declare (ignore input))
	       nil))))
#|
	       (if (not input)
		   (setf input 0.0))
	       (let ((may-fire (>= input switching-voltage)))

	       (setf cur-input input)))))
|#
