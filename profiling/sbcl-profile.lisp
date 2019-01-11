;;
;; Profiling using Time-Macro and SBCL Profiling-API
;;

(defpackage :cl-synthesizer-profiling
  (:use :cl))

(in-package :cl-synthesizer-profiling)

(require :sb-sprof)

(load "profile-rack.lisp")
(load "profile-vco.lisp")
(load "profile-phase-generator.lisp")

(defun simple-play-rack (rack &key duration-seconds)
  "Simple play function without the overhead of cl-synthesizer::play-rack"
  (let* ((update-fn (getf rack :update))
	(environment (getf rack :environment))
	(ticks (* duration-seconds (floor (getf environment :sample-rate)))))
    (dotimes (i ticks)
      (funcall update-fn nil))
    (format t "~%Executed ticks: ~a Duration: ~a seconds~%" ticks duration-seconds)))

(defparameter *jobs*
  (list
   (list
    :name "Rack Core" :disabled nil :max-samples 5000
    :time t :statistic t    
    :setup (lambda()
	     (let ((rack (cl-synthesizer-profiling-rack::make-test-rack :main-module-count 4)))
	       (values 
		(lambda ()
		  (simple-play-rack rack :duration-seconds 60))
		"Core details"
		))))
   (list
    :name "Phase Generator" :disabled nil :max-samples 500
    :time t :statistic nil
    :setup (lambda()
	     (values 
	      (lambda()
		(cl-synthesizer-profiling-phase-generator::run-phase-generator
		 :duration-seconds 1200))
	      "Phase generator details"
	      )))
   (list
    :name "VCO" :disabled nil :max-samples 5000
    :time t :statistic nil
    :setup (lambda()
	     (let ((rack (cl-synthesizer-profiling-vco::make-test-rack :vco-count 100)))
	       (values 
		(lambda ()
		  (simple-play-rack rack :duration-seconds 60))
		"VCO-Details"
		))))
   ))

;; Run job with statistical SBCL profiler
(defun profile-statistic (job)
  (let ((f (funcall (getf job :setup))))
    (sb-sprof:with-profiling
	(:max-samples (getf job :max-samples)
		      :report :flat
		      :loop t
		      :show-progress t)
      (funcall f)))
  nil)

;; Run job with time macro
(defun profile-time (job)
  (let ((f (funcall (getf job :setup))))
    (time (funcall f)))
  nil)

;; Run job with deterministic SBCL profiler
;; Not supported yet, as i have problems to set
;; job specific packages that are to be profiled :(
#|
(defun profile-deterministic (job)
  (let ((context (funcall (getf job :prepare)))
	(packages (getf job :packages)))
    (if (not packages)
	(format t "~%Skipping determistic profiling as no packages are defined~%")
	(progn
	  (sb-profile:profile "CL-SYNTHESIZER")
	  (funcall (getf job :run) context)
	  (sb-profile:report))))
  nil)
|#

(defun run-jobs ()
  (format t "~%~%################### Running profiling jobs ###################~%~%")
  (dolist (job *jobs*)
    (let ((job-name (getf job :name)))
      (if (getf job :disabled)
	  (format t "~%Skipping profiling job '~a'~%" job-name)
	  (progn 
	    (if (getf job :time)
		(progn
		  (format t "~%Running job '~a' with time profiling...~%" job-name)
		  (profile-time job)
		  (format t "~%Job '~a' with time profiling has completed~%" job-name)))
	    (if (getf job :statistic)
		(progn
		  (format t "~%Running job '~a' with statistic profiling...~%" job-name)
		  (profile-statistic job)
		  (format t "~%Job '~a' with statistic profiling has completed~%" job-name)))
	    ))))
  (format t "~%~%################### Profiling jobs completed ###################~%~%")
  "Profiling has completed")

;; (run-jobs)

