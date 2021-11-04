(let ((module (cl-synthesizer:get-module rack "VCO")))
  (let ((module-outputs (funcall (cl-synthesizer:get-outputs module))))
    (let ((sine (funcall (getf module-outputs :sine))))
      (format t "Sine: ~a" sine))))
