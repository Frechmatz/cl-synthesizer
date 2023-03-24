(defpackage :cl-synthesizer-example-1
  (:use :cl))
(in-package :cl-synthesizer-example-1)


(defun make-example-rack ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "ADDER" #'cl-synthesizer-example-1-adder-4:make-module)
    ;; add rack inputs
    (cl-synthesizer:add-rack-input rack :input-1 "ADDER" :input-1)
    (cl-synthesizer:add-rack-input rack :input-2 "ADDER" :input-2)
    (cl-synthesizer:add-rack-input rack :input-3 "ADDER" :input-3)
    (cl-synthesizer:add-rack-input rack :input-4 "ADDER" :input-4)
    ;; add rack output
    (cl-synthesizer:add-rack-output rack :sum "ADDER" :sum)
    rack))

(defun run-example ()
  (let ((rack (make-example-rack)))
    (let ((rack-inputs (funcall (getf rack :inputs)))
	  (rack-outputs (funcall (getf rack :outputs))))
      ;; set inputs
      (let ((input-setter (getf (getf rack-inputs :input-1) :set)))
	(funcall input-setter 10))
      (let ((input-setter (getf (getf rack-inputs :input-2) :set)))
	(funcall input-setter 20))
      (let ((input-setter (getf (getf rack-inputs :input-3) :set)))
	(funcall input-setter 30))
      (let ((input-setter (getf (getf rack-inputs :input-4) :set)))
	(funcall input-setter 40))
      ;; update
      (cl-synthesizer:update rack)
      ;; print output of rack
      (let ((sum (funcall (getf (getf rack-outputs :sum) :get))))
	(format t "~%Sum:~a~%" sum))
      ;; shutdown
      (cl-synthesizer:shutdown rack))))

;; (run-example)

