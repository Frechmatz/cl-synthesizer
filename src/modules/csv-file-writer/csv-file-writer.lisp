(in-package :cl-synthesizer-modules-csv-file-writer)

(defun quote-str (str str-to-quote)
  (declare (ignore str-to-quote))
  str)

(defun make-writer ()
  (let ((output-stream nil) (output-filename))
    (list
     :open-stream (lambda (filename)
		    (format t "~%Open file ~a~%" filename)
		    (setf output-filename filename)
		    (setf output-stream
			  (open
			   filename
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create
			   :external-format :utf-8))
		    output-stream)
     :close-stream (lambda()
		     (if output-stream
			 (progn
			   (format t "~%Close file ~a~%" output-filename)
			   (close output-stream)
			   (setf output-stream nil)))))))


(defvar *make-writer* (lambda (&rest args)
			(apply #'make-writer args)))

(defun make-module (name environment &key columns filename (column-separator ",") (add-header t))
  "Creates a CSV File Writer module.
    <p>The function has the following parameters:
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
  </ul></p>
  <p>The module has the following inputs:
  <ul>
      <li>:column-1 ... :column-n Where n is the number of columns.</li>
  </ul></p>
  <p>Due to performance/consing considerations all columns are written using the Lisp-Writer. 
     If a value contains the column separator it will not be quoted. The file is opened on the first 
     call of the update function and closed by the shutdown handler.
  </p>
  The module has no outputs.
  <p>The recommended way of CSV file generation is to use a Monitor.</p>"
  (if (<= (length columns) 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: Columns must not be empty."
       :format-arguments (list name)))
  (let ((column-keys (cl-synthesizer-lisp-util:make-keyword-list "column" (length columns)))
	(column-values (make-array (length columns) :initial-element nil))
	(column-properties (make-array (length columns) :initial-element nil))
	(filename (merge-pathnames filename (getf environment :home-directory)))
	(output-stream nil)
	(csv-writer (funcall *make-writer*)))
    (flet ((make-column-properties (column index)
	     (let ((c (copy-list column)))
	       (if (not (getf c :name))
		   (setf (getf c :name) (format nil "Column-~a" index)))
	       c)))
      (let ((inputs nil) (index 0))
	(dolist (column-key column-keys)
	  (let ((cur-index index))
	    (setf (aref column-properties cur-index) (make-column-properties (nth cur-index columns) index))
	    (push (lambda(value) (setf (aref column-values cur-index) value)) inputs)
	    (push column-key inputs))
	  (setf index (+ 1 index)))
      (flet ((open-file ()
	       (setf output-stream (funcall (getf csv-writer :open-stream) filename)))
	     (close-file ()
	       (funcall (getf csv-writer :close-stream)))
	     (print-header ()
	       (if add-header
		   (format output-stream "~a~%"
			   (reduce
			    (lambda(buffer column-properties)
			      (concatenate
			       'string
			       buffer
			       (if (= 0 (length buffer)) "" column-separator)
			       (quote-str (format nil "~a" (getf column-properties :name)) column-separator)))
			    column-properties
			    :initial-value "")))))
	(list
	 :inputs (lambda () inputs)
	 :outputs (lambda () nil)
	 :update (lambda ()
		   (if (not output-stream)
		       (progn
			 (open-file)
			 (print-header)))
		   (let ((is-first t))
		     (dotimes (index (length columns))
		       (let ((value (aref column-values index)))
			 (if (not is-first)
			     (write-string column-separator output-stream))
			 (setf is-first nil)
			 (if (not value)
			     (setf value (getf (aref column-properties index) :default-value)))
			 (prin1 value output-stream))))
		   (write-line "" output-stream))
	 :shutdown (lambda ()
		     (close-file))))))))
