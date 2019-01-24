
(in-package :cl-synthesizer-test)

(defun create-nested-rack (name environment)
  (declare (ignore name))
  (let ((rack (cl-synthesizer:make-rack
	       :environment environment
	       :input-sockets '(:in)
	       :output-sockets '(:out))))
    (cl-synthesizer:add-module rack "Multiplier" #'cl-synthesizer-test::multiplier-module)
    (cl-synthesizer:add-patch rack "INPUT" :in "Multiplier" :in)
    (cl-synthesizer:add-patch rack "Multiplier" :out "OUTPUT" :out)
    rack))

(define-test test-nested-rack ()
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :input-sockets '(:in)
	       :output-sockets '(:out))))
    (cl-synthesizer:add-module rack "RACK-1" #'create-nested-rack)
    (cl-synthesizer:add-module rack "RACK-2" #'create-nested-rack)
    (cl-synthesizer:add-module rack "RACK-3" #'create-nested-rack)

    (cl-synthesizer:add-patch rack "INPUT" :in "RACK-1" :in)
    (cl-synthesizer:add-patch rack "RACK-1" :out "RACK-2" :in)
    (cl-synthesizer:add-patch rack "RACK-2" :out "RACK-3" :in)
    (cl-synthesizer:add-patch rack "RACK-3" :out "OUTPUT" :out)

    (funcall (getf rack :update) (list :in 5))
    (assert-equal 40 (funcall (getf rack :get-output) :out))))
