
(in-package :cl-synthesizer-monitor-wave-handler)

(defun wave-file-handler (name environment outputs &rest rest &key filename &allow-other-keys)
  "Creates a monitor handler which writes its inputs into a Wave file.
    The function has the following arguments:
    <ul>
	<li>name A name.</li>
	<li>environment The synthesizer environment.</li>
	<li>outputs The output keys as defined by the Monitor Socket-Mapping. For now
	these must be :channel-1 ... :channel-n.</li>
	<li>:filename A file path relative to the output directory as defined by the environment.</li>
    </ul>"
  (let ((handler 
	 (apply #'cl-synthesizer-monitor-wave-file-writer:wave-file-writer
	  name
	  environment
	  :filename filename
	  :channel-count (length outputs)
	  rest)))
    ;; Validate outputs
    (dolist (output-key outputs)
      (if (not (find output-key (funcall (getf handler :inputs)) :test #'eq))
	  (cl-synthesizer:signal-assembly-error
	   :format-control "Output keyword ~a not supported by wave-file-handler"
	   :format-arguments (list output-key))))
    handler))

