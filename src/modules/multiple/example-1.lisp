(defpackage :cl-synthesizer-modules-multiple-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-multiple-example-1)

(defun example ()
  "Multiple example"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    
    (cl-synthesizer:add-module
     rack "LFO"
     #'cl-synthesizer-modules-vco:vco-linear
     :base-frequency 1.0 :v-peak 1.0 :f-max 500 :cv-max 5)
    
    (cl-synthesizer:add-module rack "MULTIPLE"
			       #'cl-synthesizer-modules-multiple:multiple :output-count 5)
    (cl-synthesizer:add-patch rack "LFO" :sine "MULTIPLE" :input)
    (cl-synthesizer:add-patch rack "MULTIPLE" :output-1 "LINE-OUT" :channel-1)
    (cl-synthesizer:add-patch rack "MULTIPLE" :output-2 "LINE-OUT" :channel-2)

    rack))

;;(cl-synthesizer:play-rack (example) 1)
