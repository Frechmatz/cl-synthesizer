(in-package :cl-synthesizer-modules-multiple)

(defun make-module (name environment &key output-count)
  "Creates a Multiple module. A multiple passes the value of exactly one input socket
   to as many output sockets as defined by output-count.
   The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:output-count The number of output sockets.</li>
    </ul>
    The module has the following inputs:
    <ul>
	<li>:input The input signal to be passed to the outputs.</li>
    </ul>
    The module has the following outputs:
    <ul>
        <li>:output-1 ... :output-n. Where n is the output-count.</li>
    </ul>"
  (declare (ignore environment))
  (if (<= output-count 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: output-count must be greater than 0: ~a"
       :format-arguments (list name output-count)))
  (let ((cur-input nil) (outputs (cl-synthesizer-macro-util:make-keyword-list "output" output-count)))
    (list
     :inputs (lambda () '(:input))
     :outputs (lambda () outputs)
     :get-output (lambda (output) (declare (ignore output)) cur-input)
     :update (lambda (&key input) (setf cur-input input)))))

