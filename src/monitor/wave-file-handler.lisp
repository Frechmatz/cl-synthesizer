
(in-package :cl-synthesizer-monitor-wave-handler)

(defun wave-file-handler (name environment inputs &rest rest &key filename &allow-other-keys)
  "Creates a monitor handler which writes its inputs into a Wave file.
    The function has the following arguments:
    <ul>
	<li>name A name.</li>
	<li>environment The synthesizer environment.</li>
	<li>inputs The inputs as defined by the Monitor Socket-Mapping. For now
	the ids of the inputs (as defined by the :id property) must be :channel-1 ... :channel-n.</li>
	<li>:filename A file path relative to the output directory as defined by the environment.</li>
    </ul>"
  (let ((handler 
	 (apply #'cl-synthesizer-monitor-wave-file-writer:wave-file-writer
	  name
	  environment
	  :filename filename
	  :channel-count (length inputs)
	  rest)))
    ;; Validate inputs
    (dolist (input inputs)
      (if (not (find (getf input :input-socket) (funcall (getf handler :inputs)) :test #'eq))
	  (cl-synthesizer:signal-assembly-error
	   :format-control "Input keyword ~a not supported by wave-file-handler"
	   :format-arguments (list (getf input :input-socket)))))
    handler))

