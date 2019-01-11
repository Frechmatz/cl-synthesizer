;;
;; A setup to measure how much time is spent in rack logic (the basic graph
;; code that iterates through the modules, collects inputs and updates nodes)
;;

(defpackage :cl-synthesizer-profiling-rack
  (:use :cl))

(in-package :cl-synthesizer-profiling-rack)

(defun make-test-module (name environment )
  "A test module with n inputs and outputs."
  (declare (ignore name environment))
  (list
   :inputs (lambda () '(:input-1 :input-2 :input-3 :input-4))
   :outputs (lambda () '(:output-1 :output-2 :output-3 :output-4))
   :get-output (lambda (output) (declare (ignore output)) 1.0)
   :update (lambda (input-args)
	     (declare (ignore input-args))
	     nil)))

(defun patch-module-module (rack m1 m2)
  (cl-synthesizer:add-patch rack m1 :output-1 m2 :input-1)
  (cl-synthesizer:add-patch rack m1 :output-2 m2 :input-2)
  (cl-synthesizer:add-patch rack m1 :output-3 m2 :input-3)
  (cl-synthesizer:add-patch rack m1 :output-4 m2 :input-4))

(defun patch-input-module (rack m)
  (cl-synthesizer:add-patch rack "INPUT" :input-1 m :input-1)
  (cl-synthesizer:add-patch rack "INPUT" :input-2 m :input-2)
  (cl-synthesizer:add-patch rack "INPUT" :input-3 m :input-3)
  (cl-synthesizer:add-patch rack "INPUT" :input-4 m :input-4))

(defun patch-output-module (rack m)
  (cl-synthesizer:add-patch rack m :output-1 "OUTPUT" :output-1)
  (cl-synthesizer:add-patch rack m :output-2 "OUTPUT" :output-2)
  (cl-synthesizer:add-patch rack m :output-3 "OUTPUT" :output-3)
  (cl-synthesizer:add-patch rack m :output-4 "OUTPUT" :output-4))

(defun make-complex-module (name environment)
  "A module implemented as a rack that contains 4 test modules that are patched with each other
   as well as the rack input/output bridge modules."
  (declare (ignore name))
  (let ((rack (cl-synthesizer:make-rack
	       :environment environment
	       :input-sockets '(:input-1 :input-2 :input-3 :input-4)
	       :output-sockets '(:output-1 :output-2 :output-3 :output-4))))
    
    (cl-synthesizer:add-module rack "MODULE-1" #'make-test-module)
    (cl-synthesizer:add-module rack "MODULE-2" #'make-test-module)
    (cl-synthesizer:add-module rack "MODULE-3" #'make-test-module)
    (cl-synthesizer:add-module rack "MODULE-4" #'make-test-module)

    (patch-input-module rack "MODULE-1")
    (patch-module-module rack "MODULE-1" "MODULE-2")
    (patch-module-module rack "MODULE-2" "MODULE-3")
    (patch-module-module rack "MODULE-3" "MODULE-4")
    (patch-output-module rack "MODULE-4")
    
    rack))

(defun make-test-rack (&key main-module-count)
  "Main rack consisting of n complex modules."
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    (dotimes (i main-module-count)
      (cl-synthesizer:add-module rack (format nil "OUTER-MODULE-~a" i) #'make-complex-module))

    rack))

