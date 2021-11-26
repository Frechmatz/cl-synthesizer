;;
;; A setup to measure how much time is spent in rack logic (the basic graph
;; code that iterates through the modules, collects inputs and updates nodes)
;;

(defpackage :cl-synthesizer-profiling-rack
  (:use :cl))

(in-package :cl-synthesizer-profiling-rack)

(defmacro make-simple-module-ctor (input-count output-count)
  "Generates a module instantiation function
   TODO: :update function should access / do something with its inputs
   TODO: :get-output function should do a switch over the socket"
  `(let ((input-sockets (cl-synthesizer-macro-util:make-keyword-list "input" ,input-count))
	 (output-sockets (cl-synthesizer-macro-util:make-keyword-list "output" ,output-count))
	 (output-values (make-array ,output-count))
	 (input-values (make-array ,input-count)))
     (let ((inputs nil) (outputs nil))
       (let ((index 0))
	 (dolist (input-socket input-sockets)
	   (let ((cur-index index))
	     (push (lambda(value) (setf (aref input-values cur-index) value)) inputs)
	     (push input-socket inputs))))
       (let ((index 0))
	 (dolist (output-socket output-sockets)
	   (let ((cur-index index))
	     (push (lambda() (aref output-values cur-index)) outputs)
	     (push output-socket outputs))))
       (lambda (name environment)
	 (declare (ignore name environment))
	 (list
	  :inputs (lambda () inputs)
	  :outputs (lambda () outputs)
	   ;; do nothing
	  :update (lambda () nil))))))

(defun patch (rack source-module-name destination-module-name socket-count)
  "Patches the outputs of a source module with the inputs of a destination module"
  (dotimes (i socket-count)
    (cl-synthesizer:add-patch rack
			      source-module-name
			      (cl-synthesizer-macro-util:make-keyword "output" i)
			      destination-module-name
			      (cl-synthesizer-macro-util:make-keyword "input" i))))

(defun patch-input (rack target-module-name socket-count)
  "Patches the inputs of the INPUT bridge module with the inputs of another module"
  (dotimes (i socket-count)
    (cl-synthesizer:expose-input-socket rack
			      (cl-synthesizer-macro-util:make-keyword "input" i)
			      target-module-name
			      (cl-synthesizer-macro-util:make-keyword "input" i))))

(defun patch-output (rack source-module-name socket-count)
  "Patches the outputs of a module with the OUTPUT bridge module"
  (dotimes (i socket-count)
    (cl-synthesizer:expose-output-socket
     rack
     (cl-synthesizer-macro-util:make-keyword "output" i)
     source-module-name
     (cl-synthesizer-macro-util:make-keyword "output" i))))

;;
;; Profiling client: Nested rack
;;

(defun make-complex-module (name environment)
  "A module implemented as a rack that contains 4 test modules that are patched with each other
   as well as the rack input/output bridge modules."
  (declare (ignore name))
  (let ((rack (cl-synthesizer:make-rack
	       :environment environment
	       :input-sockets '(:input-1 :input-2 :input-3 :input-4)
	       :output-sockets '(:output-1 :output-2 :output-3 :output-4)))
	(simple-module-ctor (make-simple-module-ctor 4 4)))

    (dotimes (i 4)
      (cl-synthesizer:add-module rack (format nil "MODULE-~a" (+ 1 i)) simple-module-ctor))
    
    (patch-input rack "MODULE-1" 4)
    (patch rack "MODULE-1" "MODULE-2" 4)
    (patch rack "MODULE-2" "MODULE-3" 4)
    (patch rack "MODULE-3" "MODULE-4" 4)
    (patch-output rack "MODULE-4" 4)
    
    rack))

;;
;; Profiling client: Nested modules
;;

(defun make-test-rack-tree (&key root-module-count)
  "Main rack consisting of n complex modules."
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    (dotimes (i root-module-count)
      (cl-synthesizer:add-module rack (format nil "OUTER-MODULE-~a" i) #'make-complex-module))

    rack))

;;
;; Profiling client: A cloud of modules (no patches)
;;

(defun make-test-rack-cloud (&key module-count input-socket-count output-socket-count)
  "Rack consisting of simple modules without patches."
  (let ((module-ctor (make-simple-module-ctor input-socket-count output-socket-count)))
    (let ((rack (cl-synthesizer:make-rack
		 :environment (cl-synthesizer:make-environment))))
      (dotimes (i module-count)
	(cl-synthesizer:add-module rack
				   (format nil "MODULE-~a" i)
				   module-ctor))

      rack)))

;;
;; Profiling client: Chain (all modules patched)
;;

(defun make-test-rack-chain (&key module-count socket-count)
  "Rack consisting of simple modules patched together in a chain."
  (let ((module-ctor (make-simple-module-ctor socket-count socket-count)))
    (let ((rack (cl-synthesizer:make-rack
		 :environment (cl-synthesizer:make-environment)
		 :input-sockets (cl-synthesizer-macro-util:make-keyword-list "input" socket-count)
		 :output-sockets (cl-synthesizer-macro-util:make-keyword-list "output" socket-count)
		 )))
      (dotimes (i module-count)
	(cl-synthesizer:add-module
	 rack
	 (format nil "MODULE-~a" (+ i 1))
	 module-ctor))
      
      (dotimes (i (+ -1 module-count))
	(patch rack
	       (format nil "MODULE-~a" (+ i 1))
	       (format nil "MODULE-~a" (+ i 2))
	       socket-count))

      (patch-input rack "MODULE-1" socket-count)
      (patch-output rack (format nil "MODULE-~a" module-count) socket-count)
      
      rack)))
