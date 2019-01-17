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

;;
;; Tooling
;;

(defun profile-statistic (fn settings)
  "Run job with statistical SBCL profiler"
  (sb-sprof:with-profiling
      (:max-samples (getf settings :max-samples)
		    :report :flat
		    :loop t
		    :show-progress t)
    (funcall fn))
  nil)

(defun profile-time (fn settings)
  "Run job with time macro"
  (declare (ignore settings))
  (time (funcall fn))
  nil)

(defun simple-play-rack (rack &key duration-seconds (rack-input nil))
  "Simple play function without the overhead of cl-synthesizer::play-rack"
  (let* ((update-fn (getf rack :update))
	(environment (getf rack :environment))
	(ticks (* duration-seconds (floor (getf environment :sample-rate)))))
    (dotimes (i ticks)
      (funcall update-fn rack-input))
    (format t "~%Play-Rack: Executed ~a ticks. Duration ~a seconds~%" ticks duration-seconds)))

(defun run-profiler (client client-lambdalist profiler-settings profiler-name profiler-fn)
  (let ((job-name (getf client :name)))
    (multiple-value-bind (fn description)
	(apply (getf client :setup) client-lambdalist)
      (format t "~%Running client '~a' with profiler ~a...~%==> ~a~%" job-name profiler-name description)
      (funcall profiler-fn fn profiler-settings)
      (format t "~%Client '~a' with profiler ~a has completed~%" job-name profiler-name))))

;;
;; Client repository (The code to be profiled)
;;

(defparameter *clients*
  (list
   (list
    :id :rack-core :name "Rack Core"
    :setup (lambda(&key duration-seconds)
	     (let ((main-module-count 4) (sub-module-count 4) (module-io-socket-count 4))
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
    :id :phase-generator :name "Phase Generator"
    :setup (lambda(&key duration-seconds)
	     (values 
	      (lambda()
		(cl-synthesizer-profiling-phase-generator::run-phase-generator
		 :duration-seconds duration-seconds))
	      (format nil "Updating phase generator for ~a seconds" duration-seconds))))
   (list
    :id :phase-sine-converter :name "Phase-Sine Converter"
    :setup (lambda(&key duration-seconds)
	     (let ((phi 10.0))
	       (values 
		(lambda()
		  (cl-synthesizer-profiling-waveform-converter::run-sine
		   :duration-seconds duration-seconds :phi phi))
		(format nil "Converting phase ~,2F to sine for ~a seconds" phi duration-seconds)))))
   (list
    :id :phase-square-converter :name "Phase-Square Converter"
    :setup (lambda(&key duration-seconds)
	     (let ((phi 10.0))
	       (values 
		(lambda()
		  (cl-synthesizer-profiling-waveform-converter::run-square
		   :duration-seconds duration-seconds :phi phi))
		(format nil "Converting phase ~,2F to square for ~a seconds" phi duration-seconds)))))
   (list
    :id :phase-triangle-converter :name "Phase-Triangle Converter"
    :setup (lambda(&key duration-seconds)
	     (let ((phi 10.0))
	       (values 
		(lambda()
		  (cl-synthesizer-profiling-waveform-converter::run-triangle
		   :duration-seconds duration-seconds :phi phi))
		(format nil "Converting phase ~,2F to triangle for ~a seconds" phi duration-seconds)))))
   (list
    :id :phase-saw-converter :name "Phase-Saw Converter"
    :setup (lambda(&key duration-seconds)
	     (let ((phi 10.0))
	       (values 
		(lambda()
		  (cl-synthesizer-profiling-waveform-converter::run-saw
		   :duration-seconds duration-seconds :phi phi))
		(format nil "Converting phase ~,2F to saw for ~a seconds" phi duration-seconds)))))
   (list
    :id :vco :name "VCO"
    :setup (lambda(&key duration-seconds vco-count)
	       (let ((rack (cl-synthesizer-profiling-vco::make-test-rack :vco-count vco-count)))
		 (let ((info (cl-synthesizer::get-rack-info rack)))
		   (values 
		    (lambda ()
		      (simple-play-rack rack :duration-seconds duration-seconds))
		    (format
		     nil
		     "Updating ~a VCOs for ~a seconds (Modules: ~a Patches: ~a)"
		     vco-count duration-seconds (getf info :module-count) (getf info :patch-count)))))))))

;;
;; Plan runner
;;

(defun run-plan (profiling-plan)
  (format t "~%###################~%Running plan '~a'~%###################~%" (getf profiling-plan :name))
  (let ((profiler-settings
	 (list
	  :max-samples (getf profiling-plan :max-samples))))
    (dolist (job (getf profiling-plan :jobs))
      (let ((client (find-if (lambda (c) (eq (getf job :client-id) (getf c :id))) *clients*)))
	(if (not client)
	    (error (format nil "Client not found: ~a" (getf job :id))))
	(let ((lambdalist (concatenate 'list (getf profiling-plan :init) (getf job :init))))
	  (if (getf profiling-plan :profile-time)
	      (run-profiler client lambdalist profiler-settings "TIME" #'profile-time))
	  (if (getf profiling-plan :profile-statistics)
	      (run-profiler client lambdalist profiler-settings "STATISTICS" #'profile-statistic))))))
  (format t "~%###################~%Plan '~a' has completed~%################### ~%" (getf profiling-plan :name))
  "Profiling has completed")

;;
;; Profiling plans
;; A plan consists of a list of clients to be profiled and profile settings.
;;

(defparameter *profiling-plan-vco-core*
  (list
   :name "Phase Generator and Waveform Converters"
   :max-samples 500
   :profile-time t
   :profile-statistics t
   :init '(:duration-seconds 3600)
   :jobs '((:client-id :phase-sine-converter :init nil)
	   (:client-id :phase-square-converter :init nil)
	   (:client-id :phase-triangle-converter :init nil)
	   (:client-id :phase-saw-converter :init nil)
	   (:client-id :phase-generator :init nil))))

(defparameter *profiling-plan-vco*
  (list
   :name "VCO"
   :max-samples 500
   :profile-time t
   :profile-statistics nil
   :init '(:duration-seconds 60 :vco-count 100)
   :jobs '((:client-id :vco :init nil))))

(defparameter *profiling-plan-rack-core*
  (list
   :name "Rack-Core"
   :max-samples 500
   :profile-time t
   :profile-statistics nil
   :init '(:duration-seconds 60)
   :jobs '((:client-id :rack-core :init nil))))

(defparameter *profiling-plan-phase-generator-100-vcos*
  (list
   :name "Phase Generator assuming 100 instances in place"
   :max-samples 500
   :profile-time t
   :profile-statistics nil
   :init (list :duration-seconds (* 60 100))
   :jobs '((:client-id :phase-generator :init nil))))

;; Measure overhead of 100 VCO Modules against corresponding Core-Calls
(defparameter *profiling-plan-vco-overhead*
  (list
   :name "Comparing 100 VCOs against core calls"
   :max-samples 500
   :profile-time t
   :profile-statistics nil
   :init nil
   :jobs '((:client-id :vco :init (:duration-seconds 60 :vco-count 100))
	   (:client-id :phase-sine-converter :init (:duration-seconds 6000))
	   (:client-id :phase-square-converter :init (:duration-seconds 6000))
	   (:client-id :phase-triangle-converter :init (:duration-seconds 6000))
	   (:client-id :phase-saw-converter :init (:duration-seconds 6000))
	   (:client-id :phase-generator :init (:duration-seconds 6000)))))

;;
;;
;;

;; (run-plan *profiling-plan-vco-core*)
;; (run-plan *profiling-plan-vco*)
;; (run-plan *profiling-plan-rack-core*)
;; (run-plan *profiling-plan-phase-generator-100-vcos*)
;; (run-plan *profiling-plan-vco-overhead*)
