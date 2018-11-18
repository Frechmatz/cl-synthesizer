
(in-package :cl-synthesizer-monitor-wave-handler)

(defun wave-file-handler (name environment inputs &rest rest &key filename &allow-other-keys)
  "Creates a monitor handler which writes its inputs into a Wave file.
    The function has the following arguments:
    <ul>
	<li>name A name.</li>
	<li>environment The synthesizer environment.</li>
	<li>inputs The inputs as defined by the Monitor Socket-Mapping.</li>
	<li>:filename A file path relative to the output directory as defined by the environment.</li>
    </ul>"
  (let ((handler 
	 (apply #'cl-synthesizer-monitor-wave-file-writer:wave-file-writer
	  name
	  environment
	  :filename filename
	  :channel-count (length inputs)
	  rest)))
    (values handler
	    ;; Ordered list of channels (we do not want to depend on order of input keys
	    ;; provided by the :inputs function of the handler.)
	    (cl-synthesizer-macro-util:make-keyword-list "channel" (length inputs)))))
	    

