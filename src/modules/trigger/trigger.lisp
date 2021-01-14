(in-package :cl-synthesizer-modules-trigger)

(defun make-module (name environment &key trigger-threshold pulse-voltage)
  "Creates a Voltage to Trigger Converter module. 
   The module fires a one clock cycle long pulse when input voltage >= trigger-threshold
   and then waits that the input voltage descends below trigger-threshold before the next
   pulse can be triggered. The module can for example be used to generate a trigger
   out of a gate signal. <p>The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:trigger-threshold The minimum value of the input which triggers a pulse.</li>
	<li>:pulse-voltage The voltage of the pulse.</li>
    </ul></p>
    <p>The module has the following inputs:
    <ul>
	<li>:input The input voltage.</li>
    </ul></p>
    <p>The module has the following outputs:
    <ul>
        <li>:output The output voltage (zero or pulse-voltage).</li>
    </ul></p>"
  (declare (ignore name environment))
  (let ((cur-output 0) (waiting nil) (input-input nil))
    (let ((inputs (list :input (lambda(value) (setf input-input value))))
	  (outputs (list :output (lambda() cur-output))))
    (list
     :inputs (lambda () inputs)
     :outputs (lambda () outputs)
     :update (lambda ()
	       (if (not input-input)
		   (setf input-input 0.0))
	       (let ((may-fire (>= input-input trigger-threshold)))
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
		    (setf cur-output 0)))))))))

