(let ((rack
       (cl-synthesizer:make-rack
	:environment ...
	:input-sockets '(:rack-input-1)
	:output-sockets '(:rack-output-1))))

  (cl-synthesizer:add-module rack "SOME-MODULE" ...)
  (cl-synthesizer:expose-input-socket rack :rack-input-1 "SOME-MODULE" :module-input)
  (cl-synthesizer:expose-output-socket rack :rack-output-1 "SOME-MODULE" :module-output))

