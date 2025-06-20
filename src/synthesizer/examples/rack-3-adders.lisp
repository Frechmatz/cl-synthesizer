(defpackage :cl-synthesizer-examples-rack-3-adders
  (:use :cl))
(in-package :cl-synthesizer-examples-rack-3-adders)


(defun make-rack ()
  "A rack holding 3 patched adders"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    ;; add modules
    (cl-synthesizer:add-module rack "ADDER-1" #'cl-synthesizer-examples-module-adder-2:make-module)
    (cl-synthesizer:add-module rack "ADDER-2" #'cl-synthesizer-examples-module-adder-2:make-module)
    (cl-synthesizer:add-module rack "MAIN-ADDER" #'cl-synthesizer-examples-module-adder-2:make-module)
    ;; patch modules
    (cl-synthesizer:add-patch rack "ADDER-1" :sum "MAIN-ADDER" :input-1)
    (cl-synthesizer:add-patch rack "ADDER-2" :sum "MAIN-ADDER" :input-2)
    ;; return the rack
    rack))

(defun run-example ()
  (make-rack))
