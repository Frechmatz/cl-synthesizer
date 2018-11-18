;;
;; Monitor handler for CSV files
;;

(in-package :cl-synthesizer-monitor-csv-handler)

(defun make-handler (name environment inputs &rest rest &key filename &allow-other-keys)
  "Creates a monitor handler which writes its inputs into a CSV file.
    The function has the following arguments:
    <ul>
	<li>name A name.</li>
	<li>environment The synthesizer environment.</li>
	<li>inputs The column input settings as provided by the Monitor component.</li>
	<li>:filename A file path relative to the output directory as defined by the environment.</li>
    </ul>
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
     (cl-synthesizer-macro-util:make-keyword-list "column" (length inputs)))))

