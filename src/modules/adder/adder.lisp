(in-package :cl-synthesizer-modules-adder)

(defun make-symbol-impl (name num package)
  (if num
      (intern (format nil "~a-~a" (string-upcase name) num) package)
      (intern (string-upcase name) package)))

(defun make-keyword (name num)
  (make-symbol-impl name num "KEYWORD"))

(defun make-keyword-list (name count)
  "Returns list of keywords ordered by number of keyword: (:<name>-1, :<name>-2, ..., <name>-<count>.
   The numbering starts by one."
  (let ((l nil))
    (dotimes (i count)
      (push (make-keyword name (+ i 1)) l))
    (nreverse l)))

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
        not of type number are ignored.</li>
    </ul></p>
    <p>The module has the following outputs:
    <ul>
	<li>:output The output consisting of the sum of the inputs.</li>
    </ul></p>"
  (declare (ignore environment))
  (if (<= input-count 0)
      (error
       'cl-synthesizer:assembly-error
       :format-control "'~a': input-count must be greater than 0: '~a'"
       :format-arguments (list name input-count)))
  (let ((cur-output nil) (input-sockets (make-keyword-list "input" input-count)))
    (let ((input-values (make-array (length input-sockets) :initial-element nil))
	  (inputs nil)
	  (outputs (list :output (list :get (lambda() cur-output)))))
      (let ((index 0))
	(dolist (input-socket input-sockets)
	  (let ((cur-index index))
	    (push (list
		   :set (lambda(value) (setf (elt input-values cur-index) value))
		   :get (lambda() (elt input-values cur-index))) inputs)
	    (push input-socket inputs))
	  (setf index (+ 1 index))))
      (list
       :inputs (lambda() inputs)
       :outputs (lambda() outputs)
       :update (lambda ()
		 (setf cur-output 0)
		 (dotimes (index (length input-values))
		   (let ((v (elt input-values index)))
		     (if (numberp v)
			 (setf cur-output (+ cur-output v))))))))))

