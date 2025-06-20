(defpackage :cl-synthesizer-examples-rack-expose
  (:use :cl))
(in-package :cl-synthesizer-examples-rack-expose)

(defun make-rack ()
  "A Rack that exposes inputs and outputs"
  (declare (ignore name))
  ;; create the rack
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    ;; add a module
    (cl-synthesizer:add-module rack "ADDER" #'cl-synthesizer-examples-module-adder-2:make-module)
    ;; expose the inputs of the module as inputs of the rack
    (cl-synthesizer:add-rack-input rack :rack-input-1 "ADDER" :input-1)
    (cl-synthesizer:add-rack-input rack :rack-input-2 "ADDER" :input-2)
    ;; expose the output of the module as output of the rack
    (cl-synthesizer:add-rack-output rack :rack-output "ADDER" :sum)
    ;; return the rack
    rack))

(defun run-example ()
  (make-rack))
