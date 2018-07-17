
(in-package :cl-synthesizer-monitor)

(defun wave-file-handler (name environment outputs &rest rest &key filename &allow-other-keys)
  "Monitor handler which writes its input into a Wave file. For now the keys
   declared by the outputs list must match the input keys as defined by the 
   wave writer module (channel-1 ... channel-n). If there is a mismatch
   an assembly error is signalled. More features to come such as mapping 
   of keys."
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((handler 
	 ;; https://stackoverflow.com/questions/2627262/how-do-i-splice-into-a-list-outside-of-a-macro-in-common-lisp
	 (apply #'cl-synthesizer-device-wave-file-writer:wave-file-writer
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

