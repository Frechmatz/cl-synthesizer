
(in-package :cl-synthesizer-monitor)

(defun wave-file-handler (name environment outputs &rest rest &key filename)
  "Monitor handler which writes its input into a Wave file. For now the outputs
   must be supported as input keywords by Wave writer module (channel-1 ... channel-n). 
   More features to come such as mapping of input-keys."
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  ;;(break)
  (let ((handler 
	 (apply (cl-synthesizer-modules-wave-file-writer:get-n-channel-wave-file-writer (length outputs))
		name
		environment
		:filename filename
		rest)))
    (dolist (output-key outputs)
      (if (not (find output-key (funcall (getf handler :inputs)) :test #'eq))
	  (cl-synthesizer:signal-assembly-error
	   :format-control "Output keyword ~a not supported by wave-file-handler"
	   :format-arguments (list output-key))))
    handler))

