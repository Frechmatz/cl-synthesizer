(in-package :cl-synthesizer-modules-multiple)

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
      (error
       'cl-synthesizer:assembly-error
       :format-control "'~a': output-count must be greater than 0: '~a'"
       :format-arguments (list name output-count)))
  (let ((cur-input nil) (output-value nil) (output-sockets (make-keyword-list "output" output-count)))
    (let ((inputs
	    (list :input (list
			  :set (lambda(value) (setf cur-input value))
			  :get (lambda() cur-input))))
	  (outputs nil))
      (let ((getter (lambda() cur-input)))
	(dolist (output-socket output-sockets)
	  (push (list :get getter) outputs)
	  (push output-socket outputs)))
      (list
       :inputs (lambda() inputs)
       :outputs (lambda() outputs)
       :update (lambda () (setf output-value cur-input))))))

