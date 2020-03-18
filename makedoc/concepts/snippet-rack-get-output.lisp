(let ((module (cl-synthesizer:get-module rack "VCO")))
  (let ((module-outputs (funcall (getf module :outputs))))
    (let ((sine (funcall (getf module-outputs :sine))))
      (format t "Sine: ~a" sine))))
