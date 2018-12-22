;;
;; A module with a fixed output value
;;


(in-package :cl-synthesizer-modules-fixed-output)

(defun make-module (name environment &key value (output-socket :out))
  "Creates a module with a fixed output value.
   The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:value The value of the module output.</li>
	<li>:output-socket Optional keyword that declares the output socket
	    identifier to be exposed by the module.</li>
    </ul>
    The module has no inputs.
    The module has one output socket according to the :output-socket argument."
  (declare (ignore name environment))
  (let ((outputs (list output-socket)))
    (list
     :inputs (lambda () nil)
     :outputs (lambda () outputs)
     :get-output (lambda (output)
		   (declare (ignore output))
		   value)
     :update (lambda (input-args) (declare (ignore input-args)) nil))))


