(let ((rack
       (cl-synthesizer:make-rack
	:environment ...
	:input-sockets '(:rack-input-1)
	:output-sockets '(:rack-output-1))))

  (cl-synthesizer:add-module rack "SOME-MODULE" ...)
  (cl-synthesizer:add-patch rack "INPUT" :rack-input-1 "SOME-MODULE" :module-input)
  (cl-synthesizer:add-patch rack "SOME-MODULE" :module-output "OUTPUT" :rack-output-1))

