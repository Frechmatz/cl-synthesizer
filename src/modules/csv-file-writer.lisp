(in-package :cl-synthesizer-modules-csv-file-writer)

(defun quote-str (str str-to-quote)
  (declare (ignore str-to-quote))
  str)

(defun make-module (name environment &key columns filename (column-separator ",") (add-header t))
  "Creates a CSV File Writer module.
    The function has the following arguments:
  <ul>
    <li>name Name of the writer.</li>
    <li>environment The synthesizer environment.</li>
    <li>:columns A list of column definitions. Each colum definition consists of a property list 
        with the following keys:
        <ul>
            <li>:name Name of the column.</li>
            <li>:format The format control-string of the column. Default value is \"~a\"</li>
            <li>:default-value Default value to be used if current column value is nil.</li>
        </ul>
    </li>
    <li>:filename The relative path of the file to be written. The filename will be concatenated
        with the base path as defined by the :home-directory property of the environment.</li>
    <li>:column-separator The column separator of the CSV file.</li>
    <li>:add-header If t then the CSV file will start with column names.</li>
  </ul>
  The module has the following inputs:
  <ul>
      <li>:column-1 ... :column-n Where n is the number of columns.</li>
  </ul>
  The module has no outputs.
  The actual csv-file is written by the :shutdown function of the module.
  <p>See also cl-synthesizer-monitor:add-monitor which provides CSV-File-Writing
     without having to add the module and the required patches to the rack.</p>"
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (if (<= (length columns) 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: Columns must not be empty."
       :format-arguments (list name)))
  (let ((rows nil)
	(column-keys nil)
	(column-properties nil))
    ;; set up input keys and column property lookup table
    (let ((i 0))
      (flet ((make-column-properties (column index)
	       (let ((c (copy-list column)))
		 (if (not (getf c :format))
		     (setf (getf c :format) "~a"))
		 (if (not (getf c :default-value))
		     (setf (getf c :default-value) ""))
		 (if (not (getf c :name))
		     (setf (getf c :name) (format nil "Column-~a" index)))
		 c)))
	(dolist (column columns)
	  (let ((column-key (cl-synthesizer-macro-util:make-keyword "column" i)))
	    (push column-key column-keys)
	    (push (make-column-properties column i) column-properties)
	    (push column-key column-properties)
	    (setf i (+ i 1))))
	;; Reverse to order in which columns will be processed
	(setf column-keys (reverse column-keys))))
    (list
     :inputs (lambda () column-keys)
     :outputs (lambda () '())
     :get-output (lambda (output) (declare (ignore output)) nil)
     :update (lambda (inputs)
	       (push inputs rows))
     :shutdown (lambda ()
		 ;;(declare (optimize (debug 3) (speed 0) (space 0))) 
		 (with-open-file (fh (merge-pathnames filename (getf environment :home-directory))
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create
		   :external-format :utf-8)
		   (if add-header
		       (format fh "~a~%"
			       (reduce
				(lambda(buffer column-key)
				  (let ((properties (getf column-properties column-key)))
				    (concatenate
				     'string
				     buffer
				     (if (= 0 (length buffer)) "" column-separator)
				     (quote-str (format nil "~a" (getf properties :name)) column-separator))))
			      column-keys
			     :initial-value "")))
		   (dolist (row (nreverse rows)) ;; This is inefficient :(
		     (let* ((is-first t) ;; State for reduce callback. 
			    (row-string
			     (reduce
			      (lambda(buffer column-key)
				(let ((value (getf row column-key))
				      (col-props (getf column-properties column-key)))
				  (if (not value)
				      (setf value (getf col-props :default-value)))
				  (let ((b (concatenate
					    'string
					    buffer
					    (if is-first "" column-separator)
					    (quote-str (format nil (getf col-props :format) value) column-separator))))
				    (setf is-first nil)
				    b)))
			      column-keys
			     :initial-value "")))
		       (format fh "~a~%" row-string))))))))
