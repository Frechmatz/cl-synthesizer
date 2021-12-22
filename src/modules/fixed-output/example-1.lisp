(defpackage :cl-synthesizer-modules-fixed-output-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-fixed-output-example-1)

(defun example ()
  "Fixed-Output example"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module
     rack "FIXED-OUTPUT"
     #'cl-synthesizer-modules-fixed-output:make-module
     :value 3.0
     :output-socket :fixed)
    
    (cl-synthesizer:expose-output-socket rack :line-out "FIXED-OUTPUT" :fixed)

    rack))

(defun run-example ()
  (cl-synthesizer:play-rack (example) :duration-seconds 1))

;; (run-example)

