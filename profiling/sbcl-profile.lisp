;;
;; Basic profiling using Time-Macro and SBCL Profiling-API
;;
;; How to run:
;; - load cl-synthesizer system
;; - load this file
;; - (run-jobs)
;;

(defpackage :cl-synthesizer-profiling
  (:use :cl))

(in-package :cl-synthesizer-profiling)

(require :sb-sprof)

(load "profile-rack.lisp")
(load "profile-vco.lisp")
(load "profile-phase-generator.lisp")

(defun simple-play-rack (rack &key duration-seconds (rack-input nil))
  "Simple play function without the overhead of cl-synthesizer::play-rack"
  (let* ((update-fn (getf rack :update))
	(environment (getf rack :environment))
	(ticks (* duration-seconds (floor (getf environment :sample-rate)))))
    (dotimes (i ticks)
      (funcall update-fn rack-input))
    (format t "~%Play-Rack: Executed ~a ticks. Duration ~a seconds~%" ticks duration-seconds)))

(defparameter *jobs*
  (list
   (list
    :name "Rack Core" :max-samples 5000
    :time t :statistic nil
    :setup (lambda()
	     (let ((main-module-count 4) (sub-module-count 4) (module-io-socket-count 4)
		   (duration-seconds 60))
	       (let ((rack (cl-synthesizer-profiling-rack::make-test-rack
			    :main-module-count main-module-count)))
		 (values 
		  (lambda ()
		    (simple-play-rack
		     rack
		     :duration-seconds duration-seconds
		     :rack-input (list :input-1 1.0 :input-2 1.0 :input-3 1.0 :input-4 1.0)))
		  (format nil "Updating ~a main-modules, ~a sub-modules and ~a i/o sockets for ~a seconds"
			  main-module-count sub-module-count module-io-socket-count duration-seconds)
		  )))))
    (list
    :name "Phase Generator" :max-samples 500
    :time nil :statistic nil
    :setup (lambda()
	     (let ((duration-seconds 1200))
	     (values 
	      (lambda()
		(cl-synthesizer-profiling-phase-generator::run-phase-generator
		 :duration-seconds duration-seconds))
	      (format nil "Updating phase generator for ~a seconds" duration-seconds)))))
   (list
    :name "VCO" :max-samples 5000
    :time nil :statistic nil
    :setup (lambda()
	     (let ((vco-count 100) (duration-seconds 60))
	     (let ((rack (cl-synthesizer-profiling-vco::make-test-rack :vco-count vco-count)))
	       (values 
		(lambda ()
		  (simple-play-rack rack :duration-seconds duration-seconds))
		(format nil "Updating ~a VCOs for ~a seconds" vco-count duration-seconds))))))))

;; Run job with statistical SBCL profiler
(defun profile-statistic (fn settings)
  (sb-sprof:with-profiling
      (:max-samples (getf settings :max-samples)
		    :report :flat
		    :loop t
		    :show-progress t)
    (funcall fn))
  nil)

;; Run job with time macro
(defun profile-time (fn settings)
  (declare (ignore settings))
  (time (funcall fn))
  nil)

(defun run-profiler (job profiler-name profiler-fn)
  (let ((job-name (getf job :name)))
    (multiple-value-bind (fn description)
	(funcall (getf job :setup))
      (format t "~%Running job '~a' with profiler ~a...~%==> ~a~%" job-name profiler-name description)
      (funcall profiler-fn fn job)
      (format t "~%Job '~a' with profiler ~a has completed~%" job-name profiler-name))))
  
(defun run-jobs ()
  (format t "~%~%################### Running profiling jobs~%~%")
  (dolist (job *jobs*)
    (if (getf job :time)
	(run-profiler job "TIME" #'profile-time))
    (if (getf job :statistic)
	(run-profiler job "STATISTICS" #'profile-statistic)))
  (format t "~%~%################### Profiling jobs completed~%~%")
  "Profiling has completed")

;; (run-jobs)

