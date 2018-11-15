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
				    (let ((column-settings (getf column :settings)))
				      (list
				       :id (getf column :input-socket)
				       :name (getf column-settings :name)
				       :format (getf column-settings :format)
				       :default-value (getf column-settings :format))))
				  inputs)
		 rest)))
    handler))

