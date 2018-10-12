(defpackage :cl-synthesizer-modules-fixed-output-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-fixed-output-example-1)

(defun example ()
  "Fixed-Output example"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module rack "FIXED-OUTPUT"
			       #'cl-synthesizer-modules-fixed-output:fixed-output
			       :value 3.0
			       :output-socket :fixed)
    (cl-synthesizer:add-patch rack "FIXED-OUTPUT" :fixed "LINE-OUT" :channel-1)

    rack))

;;(cl-synthesizer:play-rack (example) 1)
