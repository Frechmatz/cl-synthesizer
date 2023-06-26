(defpackage :cl-synthesizer-profiling-compiler
  (:use :cl))

(in-package :cl-synthesizer-profiling-compiler)

(defparameter *width* 20)
(defparameter *layers* 100)

(defun make-symbol-impl (name num package)
  (if num
      (intern (format nil "~a-~a" (string-upcase name) num) package)
      (intern (string-upcase name) package)))

(defun make-keyword (name num)
  (make-symbol-impl name num "KEYWORD"))


(defun make-module (name environment &key width)
  "Create a module with <width> number of inputs and <width> number of outputs."
  (declare (ignore name environment))
  (let ((inputs nil) (outputs nil))
    (dotimes (i width)
      ;; Define inputs
      (push
       (list
	:get (lambda() nil)
	:set (lambda(value)
	       (declare (ignore value))
	       nil))
       inputs)
      (push
       (make-keyword "INPUT" i)
       inputs)
      ;; Define outputs
      (push
       (list
	:get (lambda() nil))
	outputs)
      (push
       (make-keyword "OUTPUT" i)
       outputs))
    (list
     :inputs (lambda() inputs)
     :outputs (lambda() outputs)
     :update (lambda() nil))))
  
(defun add-aggregation-layer (rack layer-number &key width)
  "Add one module and patch inputs with <width> input-modules"
  (cl-synthesizer:add-module
     rack
     (format nil "Layer-~a" layer-number)
     #'make-module 
     :width width)
  (if (< 0 layer-number)
      (dotimes (i width)
	(cl-synthesizer:add-patch
	 rack
	 (format nil "Layer-~a-~a" (+ layer-number -1) i)
	 (make-keyword "OUTPUT" i)
	 (format nil "Layer-~a" layer-number)
	 (make-keyword "INPUT" i))))
  nil)

(defun add-spread-layer (rack layer-number &key width)
  "Add <width> modules and patch inputs with input-module"
  (dotimes (i width)
    (cl-synthesizer:add-module
     rack
     (format nil "Layer-~a-~a" layer-number i)
     #'make-module 
     :width width))
  (dotimes (i width)
    (cl-synthesizer:add-patch
     rack
     (format nil "Layer-~a" (+ layer-number -1))
     (make-keyword "OUTPUT" i)
     (format nil "Layer-~a-~a" layer-number i)
     (make-keyword "INPUT" i)))
  nil)

(defun make-test-rack ()
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    (let ((width *width*) (layers *layers*) (call-add-aggregation-layer t))
      (dotimes (layer-number layers)
	(if call-add-aggregation-layer
	    (add-aggregation-layer
	     rack
	     layer-number
	     :width width)
	    (add-spread-layer
	     rack
	     layer-number
	     :width width))
	(setf call-add-aggregation-layer (not call-add-aggregation-layer))))
    rack))

