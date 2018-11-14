(in-package :cl-synthesizer-modules-cv-to-trigger)

(defun make-module (name environment &key trigger-cv pulse-voltage)
  "Creates a Voltage to Trigger Converter module. 
   The module fires a one clock cycle long pulse when input voltage >= trigger-cv
   and then waits that the input voltage descends below trigger-cv before the next
   pulse can be triggered. The module can for example be used to generate a trigger
   out of a gate signal. The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:trigger-cv The minimum value of the input which triggers a pulse.</li>
	<li>:pulse-voltage The voltage of the pulse.</li>
    </ul>
    The module has the following inputs:
    <ul>
	<li>:input The input voltage.</li>
    </ul>
    The module has the following outputs:
    <ul>
        <li>:output The output voltage (zero or pulse-voltage).</li>
    </ul>"
  (declare (ignore name environment))
  (let ((cur-output 0) (waiting nil))
    (list
     :inputs (lambda () '(:input))
     :outputs (lambda () '(:output))
     :get-output (lambda (output) (declare (ignore output)) cur-output)
     :update (lambda (&key input)
	       (if (not input)
		   (setf input 0.0))
	       (let ((may-fire (>= input trigger-cv)))
		 (cond
		   ((and waiting may-fire)
		    (setf cur-output 0))
		   ((and waiting (not may-fire))
		    (setf cur-output 0)
		    (setf waiting nil))
		   (may-fire
		    (setf cur-output pulse-voltage)
		    (setf waiting t))
		   (t
		    (setf cur-output 0))))))))

