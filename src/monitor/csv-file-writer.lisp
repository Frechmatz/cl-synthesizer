;;
;;
;; A CSV-File-Writer
;;
;;

(in-package :cl-synthesizer-monitor-csv-file-writer)

(defun quote-str (str str-to-quote)
  (declare (ignore str-to-quote))
  str)

(defun csv-file-writer (name environment &key columns filename (column-separator ",") (add-header nil))
  "Creates a csv file writer.
    The function has the following arguments:
  <ul>
    <li>name Name of the writer.</li>
    <li>environment The synthesizer environment.</li>
    <li>:columns List of column definitions. (:id :tick :name \"Tick\" :format \"~a\" :default-value 0)</li>
    <li>:filename The relative path of the file to be written. The filename will be concatenated
        with the base path as defined by the :home-directory property of the environment.</li>
    <li>:column-separator </li>
    <li>:add-header </li>
  </ul>
  The component has input sockets as defined by the column ids.
  <ul>
  </ul>
  The component has no outputs.
  The actual csv-file is written by the :shutdown function of the component."
  ;;(declare (optimize (debug 3) (speed 0) (space 0))) 
  (if (<= (length columns) 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: Columns must not be empty."
       :format-arguments (list name)))
  (let ((rows nil)
	(inputs (mapcar (lambda(column) (getf column :id)) columns)))
    ;; Init missing values with defaults
    (setf columns (mapcar
		   (lambda(column)
		     (let ((c (copy-list column)))
		       (if (not (getf c :format))
			   (setf (getf c :format) "~a"))
		       (if (not (getf c :default-value))
			   (setf (getf c :default-value) ""))
		       (if (not (getf c :name))
			   (setf (getf c :name) (symbol-name (getf c :id))))
		       c))
		   columns))
    (list
     :inputs (lambda () inputs)
     :outputs (lambda () '())
     :get-output (lambda (output) (declare (ignore output)) nil)
     :update (lambda (&rest args)
	       (push args rows))
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
				(lambda(buffer column)
				  (concatenate
				   'string
				   buffer
				   (if (= 0 (length buffer)) "" column-separator)
				   (quote-str (format nil "~a" (getf column :name)) column-separator)))
			      columns
			     :initial-value "")))
		   (dolist (row (nreverse rows)) ;; This is inefficient :(
		     (let* ((is-first t) ;; State for reduce callback. 
			    (row-string
			     (reduce
			      (lambda(buffer column)
				(let ((value (getf row (getf column :id))))
				  (if (not value)
				      (setf value (getf row (getf column :default-value))))
				  (let ((b (concatenate
					    'string
					    buffer
					    (if is-first "" column-separator)
					    (quote-str (format nil (getf column :format) value) column-separator))))
				    (setf is-first nil)
				    b)))
			      columns
			     :initial-value "")))
		       (format fh "~a~%" row-string))))))))
