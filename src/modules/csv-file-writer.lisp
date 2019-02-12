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
            <li>:default-value Default value to be used if current column value is nil.</li>
        </ul>
    </li>
    <li>:filename The relative path of the file to be written. The filename will be concatenated
        with the base path as defined by the :home-directory property of the environment.</li>
    <li>:column-separator The column separator.</li>
    <li>:add-header If t then a header consisting of the column-names will be written.</li>
  </ul>
  The module has the following inputs:
  <ul>
      <li>:column-1 ... :column-n Where n is the number of columns.</li>
  </ul>
  <p>Due to performance/consing considerations all columns are written using the Lisp-Writer. 
     If a value contains the column separator it will not be quoted. The file is opened on the first 
     call of the update function and closed by the shutdown handler.
  </p>
  The module has no outputs.
  <p>See also cl-synthesizer-monitor:add-monitor</p>"
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (if (<= (length columns) 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: Columns must not be empty."
       :format-arguments (list name)))
  (let ((column-keys nil)
	(column-properties nil)
	(filename (merge-pathnames filename (getf environment :home-directory)))
	(output-stream nil))
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
	;; Reverse to order in which columns will be printed
	(setf column-keys (reverse column-keys)))
      (flet ((open-file ()
	       (format t "~%Open file ~a~%" filename)
	       (setf output-stream
		     (open
		      filename
		      :direction :output
		      :if-exists :supersede
		      :if-does-not-exist :create
			  :external-format :utf-8)))
	     (close-file ()
	       (if output-stream
		   (progn
		     (format t "~%Close file ~a~%" filename)
		     (close output-stream)
		     (setf output-stream nil))))
	     (print-header ()
	       (if add-header
		   (format output-stream "~a~%"
			   (reduce
			    (lambda(buffer column-key)
			      (let ((properties (getf column-properties column-key)))
				(concatenate
				 'string
				 buffer
				 (if (= 0 (length buffer)) "" column-separator)
				 (quote-str (format nil "~a" (getf properties :name)) column-separator))))
			    column-keys
			    :initial-value ""))))
	     (print-values (row)
	       (let ((is-first t))
		 (dolist (column-key column-keys)
		   (let ((col-props (getf column-properties column-key))
			 (value (getf row column-key)))
		     (if (not is-first)
			 (write-string column-separator output-stream))
		     (setf is-first nil)
		     (if (not value)
			 (setf value (getf col-props :default-value)))
		     (prin1 value output-stream)
		     )))
	       (write-line "" output-stream)))
	(list
	 :inputs (lambda () column-keys)
	 :outputs (lambda () '())
	 :get-output (lambda (output) (declare (ignore output)) nil)
	 :update (lambda (inputs)
		   (if (not output-stream)
		       (progn
			 (open-file)
			 (print-header)))
		   (print-values inputs))
	 :shutdown (lambda ()
		     (close-file)))))))
