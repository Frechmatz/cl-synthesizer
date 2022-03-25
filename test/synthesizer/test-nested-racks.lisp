
(in-package :cl-synthesizer-test)

(defun create-nested-rack (name environment)
  (declare (ignore name))
  (let ((rack (cl-synthesizer:make-rack
	       :environment environment)))
    (cl-synthesizer:add-module rack "Multiplier" #'cl-synthesizer-test::multiplier-module)
    (cl-synthesizer:expose-input-socket rack :in "Multiplier" :in)
    (cl-synthesizer:expose-output-socket rack :out "Multiplier" :out)
    
    rack))

(define-test test-nested-rack ()
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "RACK-1" #'create-nested-rack)
    (cl-synthesizer:add-module rack "RACK-2" #'create-nested-rack)
    (cl-synthesizer:add-module rack "RACK-3" #'create-nested-rack)

    (cl-synthesizer:expose-input-socket rack :in "RACK-1" :in)
    (cl-synthesizer:add-patch rack "RACK-1" :out "RACK-2" :in)
    (cl-synthesizer:add-patch rack "RACK-2" :out "RACK-3" :in)
    (cl-synthesizer:expose-output-socket rack :out "RACK-3" :out)

    (update-module rack (list (list :in 5)))
    (assert-equal 40 (get-module-output rack :out))))
