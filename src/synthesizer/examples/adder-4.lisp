(defpackage :cl-synthesizer-examples-adder-4
  (:use :cl)
  (:export :make-module))
(in-package :cl-synthesizer-examples-adder-4)

(defun make-module (name environment)
  "Adder module with 4 inputs"
  (declare (ignore name))
  ;; create a rack
  (let ((rack (cl-synthesizer:make-rack :environment environment)))

    ;; add modules
    (cl-synthesizer:add-module rack "ADDER-1" #'cl-synthesizer-examples-adder-2:make-module)
    (cl-synthesizer:add-module rack "ADDER-2" #'cl-synthesizer-examples-adder-2:make-module)
    (cl-synthesizer:add-module rack "MAIN-ADDER" #'cl-synthesizer-examples-adder-2:make-module)

    ;; patch modules
    (cl-synthesizer:add-patch rack "ADDER-1" :sum "MAIN-ADDER" :input-1)
    (cl-synthesizer:add-patch rack "ADDER-2" :sum "MAIN-ADDER" :input-2)
    
    ;; define rack inputs
    (cl-synthesizer:add-rack-input rack :input-1 "ADDER-1" :input-1)
    (cl-synthesizer:add-rack-input rack :input-2 "ADDER-1" :input-2)
    (cl-synthesizer:add-rack-input rack :input-3 "ADDER-2" :input-1)
    (cl-synthesizer:add-rack-input rack :input-4 "ADDER-2" :input-2)

    ;; define rack output
    (cl-synthesizer:add-rack-output rack :sum "MAIN-ADDER" :sum)

    ;; return the rack
    rack))
