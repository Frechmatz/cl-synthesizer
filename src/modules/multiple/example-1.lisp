(defpackage :cl-synthesizer-modules-multiple-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-multiple-example-1)

(defun example ()
  "Multiple example"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :output-sockets '(:line-out-1 :line-out-2))))
    
    (cl-synthesizer:add-module
     rack "LFO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 1.0 :v-peak 1.0 :f-max 500.0 :cv-max 5.0)
    
    (cl-synthesizer:add-module rack "MULTIPLE"
			       #'cl-synthesizer-modules-multiple:make-module :output-count 5)
    (cl-synthesizer:add-patch rack "LFO" :sine "MULTIPLE" :input)
    (cl-synthesizer:add-patch rack "MULTIPLE" :output-1 "OUTPUT" :line-out-1)
    (cl-synthesizer:add-patch rack "MULTIPLE" :output-2 "OUTPUT" :line-out-2)

    rack))

(defun run-example ()
  (let ((rack (example)))
    (cl-synthesizer:play-rack rack :duration-seconds 10)))

;; (run-example)
