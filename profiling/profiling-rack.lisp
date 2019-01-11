;;
;; A setup to measure how much time is spent in rack logic (the basic graph
;; code that iterates through the modules, collects inputs and updates nodes)
;;

(require :sb-sprof)

(defpackage :cl-synthesizer-playground-profiling-rack
  (:use :cl))

(in-package :cl-synthesizer-playground-profiling-rack)

(defun make-test-module (name environment )
  "A test module with n inputs and outputs. The update function does nothing."
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

(defun make-test-rack ()
  "Main rack consisting of n complex modules."
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))

    (dotimes (i 4)
      (cl-synthesizer:add-module rack (format nil "OUTER-MODULE-~a" i) #'make-complex-module))

    rack))

(defun simple-play-rack (rack duration-seconds)
  "Simple play function to exclude out a perhaps bad implementaion of the cl-synthesizer::play-rack function"
  (let* ((update-fn (getf rack :update))
	(environment (getf rack :environment))
	(ticks (* duration-seconds (floor (getf environment :sample-rate)))))
    (dotimes (i ticks)
      (funcall update-fn nil))
    (format t "~%Executed ticks: ~a~%" ticks)))

(defparameter *use-simple-play-rack* t)
(defparameter *rack-play-time-seconds* 60)

(defun play (rack)
  (let ((start-time (get-internal-real-time)))
    (if *use-simple-play-rack*
	(progn
	  (simple-play-rack rack *rack-play-time-seconds*)
	  (format t "~%Simple play rack done. Elapsed time: ~,5F seconds~%"
		  (/ (- (get-internal-real-time) start-time) INTERNAL-TIME-UNITS-PER-SECOND)) 
	  (format nil "Simple play rack done."))
	(progn 
	  (cl-synthesizer:play-rack rack *rack-play-time-seconds*)
	  (format t "~%cl-synthesizer::play-rack done. Elapsed time: ~,5F seconds~%"
		  (/ (- (get-internal-real-time) start-time) INTERNAL-TIME-UNITS-PER-SECOND))
	  (format nil "cl-synthesizer::play-rack done.")))))


;;
;; ------------------------------------------------
;; Run rack with deterministic SBCL profiler
;; ------------------------------------------------
;;
#| 
(defun profile-deterministic-rack ()
  (sb-profile:unprofile "CL-SYNTHESIZER" "CL-SYNTHESIZER-PLAYGROUND-PROFILING-RACK")
  (let ((rack (make-test-rack)))
      (sb-profile:profile "CL-SYNTHESIZER" "CL-SYNTHESIZER-PLAYGROUND-PROFILING-RACK")
      (play rack)
      (sb-profile:report))
   "Deterministic profiling done")

;;(profile-deterministic-rack)
|#

;;
;; ------------------------------------------------
;; Run rack with statistical SBCL profiler
;; ------------------------------------------------
;;
#| 
(defun profile-statistic-rack ()
  (sb-profile:unprofile "CL-SYNTHESIZER" "CL-SYNTHESIZER-PLAYGROUND-PROFILING-RACK")
  (let ((rack (make-test-rack)))
      (sb-sprof:with-profiling (:max-samples 5000
                          :report :flat
                          :loop t
                          :show-progress t)
         (play rack)))
   "Statistical profiling done")

;;(profile-statistic-rack)
|#


;;
;; ------------------------------------------------
;; Run rack with simple profiling provided by time macro
;; ------------------------------------------------
;;
#| 
(defun measure-rack-time ()
;;  (sb-profile:unprofile "CL-SYNTHESIZER")
  (let ((rack (make-test-rack))) (time (play rack))))

;; (measure-rack-time)
|#

;;
;; ------------------------------------------------
;; Run rack without any profiling
;; ------------------------------------------------
;;
#|
(defun play-without-profiling ()
  (sb-profile:unprofile "CL-SYNTHESIZER")
  (let ((rack (make-test-rack))) (play rack)))

;; (play-without-profiling)
|#


