
(in-package :cl-synthesizer-monitor)

(defun wave-file-handler (name environment outputs &rest rest &key filename)
  "Monitor handler which writes its input into a Wave file. More features to come
  such as validation checks and mapping of input-keys."
  (apply (cl-synthesizer-modules-wave-file-writer:get-n-channel-wave-file-writer (length outputs))
	 name
	 environment
	 :filename filename
	 rest))

