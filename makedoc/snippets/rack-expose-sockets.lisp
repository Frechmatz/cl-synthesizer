(let ((rack
       (cl-synthesizer:make-rack :environment ...)))
  (cl-synthesizer:add-module rack "SOME-MODULE" ...)
  (cl-synthesizer:add-rack-input rack :rack-input-1 "SOME-MODULE" :module-input)
  (cl-synthesizer:add-rack-output rack :rack-output-1 "SOME-MODULE" :module-output))

