(in-package :cl-synthesizer-monitor-buffer-agent)

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

(defun make-buffer-module (name environment &key buffer)
  (declare (ignore environment))
  (let ((count (length buffer)))
    (if (<= count 0)
	(error
	 'cl-synthesizer:assembly-error
	 :format-control "'~a': Length of buffer must be greater than 0: '~a'"
	 :format-arguments (list name count)))
    (let ((input-sockets (make-keyword-list
			  "input" count)))
      (let ((inputs nil) (index 0))
	(dolist (input-socket input-sockets)
	  (let ((cur-index index))
	    (push (list
		   :set (lambda(value) (setf (aref buffer cur-index) value))
		   :get (lambda() (aref buffer cur-index))) inputs)
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
  "Creates a monitor backend which writes to a memory buffer.
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
     (make-keyword-list "input" (length inputs)))))

