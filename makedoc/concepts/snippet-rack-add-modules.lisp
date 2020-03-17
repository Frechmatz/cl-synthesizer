(let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
  (cl-synthesizer:add-module rack "LFO" ...)
  (cl-synthesizer:add-module rack "VCO" ...)
  (cl-synthesizer:add-patch rack "LFO" :sine "VCO" :cv-lin))
