(defpackage :cl-synthesizer-modules-fixed-output-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-fixed-output-example-1)

(defun example ()
  "Fixed-Output example"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :output-sockets '(:line-out))))
    
    (cl-synthesizer:add-module rack "FIXED-OUTPUT"
			       #'cl-synthesizer-modules-fixed-output:fixed-output
			       :value 3.0
			       :output-socket :fixed)
    (cl-synthesizer:add-patch rack "FIXED-OUTPUT" :fixed "OUTPUT" :line-out)

    rack))

;;(cl-synthesizer:play-rack (example) 1)
