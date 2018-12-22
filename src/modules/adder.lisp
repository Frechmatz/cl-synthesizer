(in-package :cl-synthesizer-modules-adder)

(defun make-module (name environment &key input-count)
  "Creates a simple voltage adder module. The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:input-count The number of input sockets.</li>
    </ul>
    The module has the following inputs:
    <ul>
        <li>:input-1 ... :input-n. Where n is the input-count. Input values
        not of type <b>number</b> are ignored.</li>
    </ul>
    The module has the following outputs:
    <ul>
	<li>:output The output consisting of the sum of the inputs.</li>
    </ul>"
  (declare (ignore environment))
  (if (<= input-count 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: input-count must be greater than 0: ~a"
       :format-arguments (list name input-count)))
  (let ((cur-output nil) (inputs (cl-synthesizer-macro-util:make-keyword-list "input" input-count)))
    (list
     :inputs (lambda () inputs)
     :outputs (lambda () '(:output))
     :get-output (lambda (output) (declare (ignore output)) cur-output)
     :update (lambda (inputs)
	       (setf cur-output 0)
	       (dolist (i inputs)
		 (if (numberp i)
		     (setf cur-output (+ cur-output i))))))))

