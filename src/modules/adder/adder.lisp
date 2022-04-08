(in-package :cl-synthesizer-modules-adder)

(defun make-module (name environment &key input-count)
  "Creates a simple voltage adder module. 
   <p>The function has the following parameters:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:input-count The number of input sockets.</li>
    </ul></p>
    <p>The module has the following inputs:
    <ul>
        <li>:input-1 ... :input-n. Where n is the input-count. Input values
        not of type <b>number</b> are ignored.</li>
    </ul></p>
    <p>The module has the following outputs:
    <ul>
	<li>:output The output consisting of the sum of the inputs.</li>
    </ul></p>"
  (declare (ignore environment))
  (if (<= input-count 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: input-count must be greater than 0: ~a"
       :format-arguments (list name input-count)))
  (let ((cur-output nil) (input-sockets (cl-synthesizer-lisp-util:make-keyword-list "input" input-count)))
    (let ((input-values (make-array (length input-sockets) :initial-element nil))
	  (inputs nil)
	  (outputs (list :output (lambda() cur-output))))
      (let ((index 0))
	(dolist (input-socket input-sockets)
	  (let ((cur-index index))
	    (push (lambda(value) (setf (elt input-values cur-index) value)) inputs) 
	    (push input-socket inputs))
	  (setf index (+ 1 index))))
      (list
       :inputs (lambda () inputs)
       :outputs (lambda () outputs)
       :update (lambda ()
		 (setf cur-output 0)
		 (dotimes (index (length input-values))
		   (let ((v (elt input-values index)))
		     (if (numberp v)
			 (setf cur-output (+ cur-output v))))))))))

