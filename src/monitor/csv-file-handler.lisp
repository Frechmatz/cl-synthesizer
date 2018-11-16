;;
;; Monitor handler for CSV files
;;

(in-package :cl-synthesizer-monitor-csv-handler)

(defun csv-file-handler (name environment inputs &rest rest &key filename &allow-other-keys)
  "Creates a monitor handler which writes its inputs into a CSV file.
    The function has the following arguments:
    <ul>
	<li>name A name.</li>
	<li>environment The synthesizer environment.</li>
	<li>inputs The input keys as provided by the Monitor component.</li>
	<li>:filename A file path relative to the output directory as defined by the environment.</li>
    </ul>"
  (let* ((handler 
	  (apply #'cl-synthesizer-monitor-csv-file-writer:csv-file-writer
		 name
		 environment
		 :filename filename
		 :columns (mapcar (lambda(column)
				    (getf column :settings))
				  inputs)
		 rest)))
    ;; Validate inputs
    (dolist (input inputs)
      (if (not (find (getf input :input-socket) (funcall (getf handler :inputs)) :test #'eq))
	  (cl-synthesizer:signal-assembly-error
	   :format-control "Input keyword ~a not supported by csv-file-handler"
	   :format-arguments (list (getf input :input-socket)))))
    handler))

