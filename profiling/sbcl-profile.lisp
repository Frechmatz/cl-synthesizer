;;
;; Basic profiling using Time-Macro and SBCL Profiling-API
;;
;; How to run:
;; - load system cl-synthesizer-profiling
;; - (run-jobs)
;;

(defpackage :cl-synthesizer-profiling
  (:use :cl))

(in-package :cl-synthesizer-profiling)

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
    :time nil :statistic nil
    :setup (lambda()
	     (let ((main-module-count 4) (sub-module-count 4) (module-io-socket-count 4)
		   (duration-seconds 60))
	       (let ((rack (cl-synthesizer-profiling-rack::make-test-rack
			    :main-module-count main-module-count)))
		 (let ((info (cl-synthesizer::get-rack-info rack)))
		   (values 
		    (lambda ()
		      (simple-play-rack
		       rack
		       :duration-seconds duration-seconds))
		    (format
		     nil
		     "Updating ~a main-modules, ~a sub-modules and ~a i/o sockets for ~a seconds (Modules: ~a Patches: ~a)"
		     main-module-count sub-module-count module-io-socket-count duration-seconds
		     (getf info :module-count) (getf info :patch-count))
		    ))))))
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
    :name "Phase-Sine Converter" :max-samples 500
    :time t :statistic nil
    :setup (lambda()
	     (let ((duration-seconds 60) (phi 10.0))
	       (values 
		(lambda()
		  (cl-synthesizer-profiling-waveform-converter::run-sine
		   :duration-seconds duration-seconds :phi phi))
		(format nil "Converting phase ~,2F to sine for ~a seconds" phi duration-seconds)))))
   (list
    :name "Phase-Square Converter" :max-samples 500
    :time t :statistic nil
    :setup (lambda()
	     (let ((duration-seconds 60) (phi 10.0))
	       (values 
		(lambda()
		  (cl-synthesizer-profiling-waveform-converter::run-square
		   :duration-seconds duration-seconds :phi phi))
		(format nil "Converting phase ~,2F to square for ~a seconds" phi duration-seconds)))))
   (list
    :name "Phase-Triangle Converter" :max-samples 500
    :time t :statistic nil
    :setup (lambda()
	     (let ((duration-seconds 60) (phi 10.0))
	       (values 
		(lambda()
		  (cl-synthesizer-profiling-waveform-converter::run-triangle
		   :duration-seconds duration-seconds :phi phi))
		(format nil "Converting phase ~,2F to triangle for ~a seconds" phi duration-seconds)))))
   (list
    :name "Phase-Saw Converter" :max-samples 500
    :time t :statistic nil
    :setup (lambda()
	     (let ((duration-seconds 60) (phi 10.0))
	       (values 
		(lambda()
		  (cl-synthesizer-profiling-waveform-converter::run-saw
		   :duration-seconds duration-seconds :phi phi))
		(format nil "Converting phase ~,2F to saw for ~a seconds" phi duration-seconds)))))
   (list
    :name "VCO" :max-samples 5000
    :time nil :statistic nil
    :setup (lambda()
	     (let ((vco-count 100) (duration-seconds 60))
	       (let ((rack (cl-synthesizer-profiling-vco::make-test-rack :vco-count vco-count)))
		 (let ((info (cl-synthesizer::get-rack-info rack)))
		   (values 
		    (lambda ()
		      (simple-play-rack rack :duration-seconds duration-seconds))
		    (format
		     nil
		     "Updating ~a VCOs for ~a seconds (Modules: ~a Patches: ~a)"
		     vco-count duration-seconds (getf info :module-count) (getf info :patch-count))))))))))

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

