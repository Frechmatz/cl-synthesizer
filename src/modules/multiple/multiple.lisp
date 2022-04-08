(in-package :cl-synthesizer-modules-multiple)

(defun make-module (name environment &key output-count)
  "Creates a Multiple module. A multiple passes the value of exactly one input socket
   to as many output sockets as defined by output-count.
   <p>The function has the following parameters:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:output-count The number of output sockets.</li>
    </ul></p>
    <p>The module has the following inputs:
    <ul>
	<li>:input The input signal to be passed to the outputs.</li>
    </ul></p>
    <p>The module has the following outputs:
    <ul>
        <li>:output-1 ... :output-n. Where n is the output-count.</li>
    </ul></p>"
  (declare (ignore environment))
  (if (<= output-count 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: output-count must be greater than 0: ~a"
       :format-arguments (list name output-count)))
  (let ((cur-input nil) (output-value nil) (output-sockets (cl-synthesizer-lisp-util:make-keyword-list "output" output-count)))
    (let ((inputs
	   (list :input (lambda(value) (setf cur-input value))))
	  (outputs nil))
      (let ((getter (lambda() cur-input)))
	(dolist (output-socket output-sockets)
	  (push getter outputs)
	  (push output-socket outputs)))
      (list
       :inputs (lambda () inputs)
       :outputs (lambda () outputs)
       :update (lambda () (setf output-value cur-input))))))

