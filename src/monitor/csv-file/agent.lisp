(in-package :cl-synthesizer-monitor-csv-file-agent)

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

(defun make-backend (name environment inputs &rest rest &key filename &allow-other-keys)
  "Creates a monitor backend which writes its inputs into a CSV file.
    <p>The function has the following parameters:
    <ul>
	<li>name A name.</li>
	<li>environment The synthesizer environment.</li>
	<li>inputs The column input settings as provided by the Monitor component.</li>
	<li>:filename A file path relative to the output directory as defined by the environment.</li>
    </ul></p>
    <p>The function returns a values object consisting of
    <ul>
       <li>A property list that implements a module (this is the Monitor-Backend).</li>
       <li>An ordered list of input sockets of the module.</li>
    </ul></p>
    <p>See also cl-synthesizer-modules:csv-file-writer.</p>"
  (let* ((handler 
	  (apply #'cl-synthesizer-modules-csv-file-writer:make-module
		 name
		 environment
		 :filename filename
		 :columns inputs
		 rest)))
    (values 
     handler
     ;; Ordered list of channels (we do not want to depend on order of input keys
     ;; provided by the :inputs function of the csv file writer module.)
     (make-keyword-list "column" (length inputs)))))

