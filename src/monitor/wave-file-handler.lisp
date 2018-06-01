
(in-package :cl-synthesizer-monitor)

(defun wave-file-handler (name environment outputs &rest rest &key filename)
  "Monitor handler which writes its input into a Wave file. For now the keys
   declared by the outputs list must match the input keys as defined by the 
   wave writer module (channel-1 ... channel-n). If there is a mismatch
   an assembly error is signalled. More features to come such as mapping 
   of keys."
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

