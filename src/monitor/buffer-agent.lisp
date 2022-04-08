(in-package :cl-synthesizer-monitor-buffer-agent)


(defun make-buffer-module (name environment &key buffer)
  (declare (ignore environment))
  (let ((count (length buffer)))
    (if (<= count 0)
	(cl-synthesizer:signal-assembly-error
	 :format-control "~a: Length of buffer must be greater than 0: ~a"
	 :format-arguments (list name count)))
    (let ((input-sockets (cl-synthesizer-lisp-util:make-keyword-list
			  "input" count)))
      (let ((inputs nil) (index 0))
	(dolist (input-socket input-sockets)
	  (let ((cur-index index))
	    (push (lambda(value) (setf (aref buffer cur-index) value)) inputs)
	    (push input-socket inputs)
	    (setf index (+ 1 index))))
	(list
	 :inputs (lambda () inputs)
	 :outputs (lambda () nil)
	 :update (lambda ()))))))

;;
;;
;;

(defun make-backend (name environment inputs &rest rest &key buffer &allow-other-keys)
  "Creates a monitor backend which writes its inputs into a buffer.
    <p>The function has the following parameters:
    <ul>
	<li>name A name.</li>
	<li>environment The synthesizer environment.</li>
	<li>inputs The input settings as provided by the Monitor component.</li>
	<li>:buffer An array with dimension (length inputs).</li>
    </ul></p>
    <p>The function returns a values object consisting of
    <ul>
       <li>A property list that implements a module (this is the Monitor-Backend).</li>
       <li>An ordered list of input sockets of the module.</li>
    </ul></p>"
  (declare (ignore rest))
  (let ((handler (make-buffer-module name environment :buffer buffer)))
    (values 
     handler
     (cl-synthesizer-lisp-util:make-keyword-list "input" (length inputs)))))

