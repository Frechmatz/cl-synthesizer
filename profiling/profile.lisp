;;
;; Basic profiling using Time-Macro and SBCL Profiling-API
;;
;; How to run:
;; - load system cl-synthesizer-profiling
;; - Execute (run-plan plan) with any profiling plan
;;

(defpackage :cl-synthesizer-profiling
  (:use :cl))

(in-package :cl-synthesizer-profiling)

;;
;; Tooling
;;

(defun get-rack-info (rack)
  (let ((module-count 0) (patch-count (length (cl-synthesizer:get-patches rack))))
    (let ((modules (mapcar (lambda(name) (cl-synthesizer:get-module rack name))
			   (cl-synthesizer:get-modules rack))))
      ;; Added modules + INPUT + OUTPUT
      (dolist (module modules)
	(setf module-count (+ module-count 1))
	(let ((module module))
	  (if (cl-synthesizer:is-rack module)
	      (let ((info (get-rack-info module)))
		(setf module-count (+ module-count (getf info :module-count)))
		(setf patch-count (+ patch-count (getf info :patch-count)))))))
      (list
       :module-count module-count
       :patch-count patch-count))))

(defun profile-statistical (fn settings)
  "Run job with statistical profiler (if any provided by lisp implementation"
#+:sbcl (sb-sprof:with-profiling
      (:max-samples (getf settings :max-samples)
		    :report :flat
		    :loop t
		    :show-progress t)
	  (funcall fn))
#-:sbcl (format t "~%WARN: Statistical profiling not supported by lisp implementation~%")
  nil)

(defun profile-time (fn settings)
  "Run job with time macro"
  (declare (ignore settings))
  (time (funcall fn))
  nil)

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
		 (let ((info (get-rack-info rack)))
		   (values 
		    (lambda ()
		      (cl-synthesizer:play-rack
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
		 (let ((info (get-rack-info rack)))
		   (values 
		    (lambda ()
		      (cl-synthesizer:play-rack rack :duration-seconds duration-seconds))
		    (format
		     nil
		     "Updating ~a VCOs for ~a seconds (Modules: ~a Patches: ~a)"
		     vco-count duration-seconds (getf info :module-count) (getf info :patch-count)))))))
   (list
    :id :monitor :name "Monitor"
    :setup (lambda(&key duration-seconds)
	     (let ((rack (cl-synthesizer-profiling-monitor::make-test-rack)))
	       (let ((info (get-rack-info rack)))
		 (values 
		  (lambda ()
		    (cl-synthesizer:play-rack rack :duration-seconds duration-seconds))
		  (format
		   nil
		   "Calling monitor for ~a seconds (Modules: ~a Patches: ~a)"
		   duration-seconds (getf info :module-count) (getf info :patch-count)))))))
   (list
    :id :midi-sequencer :name "Midi-Sequencer"
    :setup (lambda(&key duration-seconds)
	     (let ((rack (cl-synthesizer-profiling-midi-sequencer::make-test-rack)))
	       (let ((info (get-rack-info rack)))
		 (values 
		  (lambda ()
		    (cl-synthesizer:play-rack rack :duration-seconds duration-seconds))
		  (format
		   nil
		   "Calling midi-sequencer for ~a seconds (Modules: ~a Patches: ~a)"
		   duration-seconds (getf info :module-count) (getf info :patch-count)))))))
   (list
    :id :csv-writer :name "CSV-Writer"
    :setup (lambda(&key duration-seconds)
	     (let ((rack (cl-synthesizer-profiling-csv-file-writer::make-test-rack)))
	       (values 
		(lambda ()
		  (cl-synthesizer:play-rack rack :duration-seconds duration-seconds))
		(format
		 nil
		 "Updating CSV-Writer for ~a seconds" duration-seconds)))))
   (list
    :id :wave-writer :name "Wave-Writer"
    :setup (lambda(&key duration-seconds sample-rate filename)
	     (let ((rack (cl-synthesizer-profiling-wave-file-writer::make-test-rack
			  :sample-rate sample-rate
			  :filename filename)))
	       (values 
		(lambda ()
		  (cl-synthesizer:play-rack rack :duration-seconds duration-seconds))
		(format
		 nil
		 "Updating Wave-Writer for ~a seconds with sample-rate ~a into file ~a"
		 duration-seconds sample-rate filename)))))

   (list
    :id :midi-interface :name "MIDI-Interface"
    :setup (lambda(&key duration-seconds)
	     (let ((update-fn (cl-synthesizer-profiling-midi-interface::init)))
	       (values 
		(lambda ()
		  (funcall update-fn :duration-seconds duration-seconds))
		(format
		 nil
		 "Updating MIDI-Interface for ~a seconds" duration-seconds)))))
   
   ))

;;
;; Profiling plan runner
;;

(defun run-plan (profiling-plan)
  (format t "~%###################~%Running plan '~a'~%###################~%" (getf profiling-plan :name))
  (let ((profiler-settings
	 (list
	  :max-samples (getf profiling-plan :max-samples))))
    (dolist (job (getf profiling-plan :jobs))
      (let ((client (find-if (lambda (c) (eq (getf job :client-id) (getf c :id))) *clients*)))
	(if (not client)
	    (error (format nil "Client not found: ~a" (getf job :client-id))))

	(let ((lambdalist (concatenate 'list (getf profiling-plan :init) (getf job :init))))
	  (if (getf profiling-plan :profile-time)
	      (run-profiler client lambdalist profiler-settings "TIME" #'profile-time))
	  (if (getf profiling-plan :profile-statistical)
	      (run-profiler client lambdalist profiler-settings "STATISTICAL" #'profile-statistical))))))
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
   :profile-statistical nil
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
   :profile-statistical nil
   :init '(:duration-seconds 60 :vco-count 100)
   :jobs '((:client-id :vco :init nil))))

(defparameter *profiling-plan-rack-core*
  (list
   :name "Rack-Core"
   :max-samples 500
   :profile-time t
   :profile-statistical nil
   :init nil
   :jobs '((:client-id :rack-core :init (:duration-seconds 60))
	   (:client-id :rack-core :init (:duration-seconds 600)))))

(defparameter *profiling-plan-vco-overhead*
  (list
   :name "Measure overhead of 100 VCO Modules against corresponding Core-Calls"
   :max-samples 500
   :profile-time t
   :profile-statistical nil
   :init nil
   :jobs '((:client-id :vco :init (:duration-seconds 60 :vco-count 100))
	   (:client-id :phase-sine-converter :init (:duration-seconds 6000))
	   (:client-id :phase-square-converter :init (:duration-seconds 6000))
	   (:client-id :phase-triangle-converter :init (:duration-seconds 6000))
	   (:client-id :phase-saw-converter :init (:duration-seconds 6000))
	   (:client-id :phase-generator :init (:duration-seconds 6000)))))

(defparameter *profiling-plan-monitor*
  (list
   :name "Monitor"
   :max-samples 500
   :profile-time t
   :profile-statistical nil
   :init '(:duration-seconds 120)
   :jobs '((:client-id :monitor :init nil))))

(defparameter *profiling-plan-midi-sequencer*
  (list
   :name "Midi-Sequencer"
   :max-samples 500
   :profile-time t
   :profile-statistical nil
   :init '(:duration-seconds 3600)
   :jobs '((:client-id :midi-sequencer :init nil))))

(defparameter *profiling-plan-csv-writer*
  (list
   :name "CSV-Writer"
   :max-samples 500
   :profile-time t
   :profile-statistical nil
   :init nil
   :jobs '((:client-id :csv-writer :init (:duration-seconds 60)))))

(defparameter *profiling-plan-wave-writer*
  (list
   :name "Wave-Writer"
   :max-samples 500
   :profile-time t
   :profile-statistical nil
   :init nil
   :jobs '(
	   (:client-id :wave-writer :init
	    (:duration-seconds 30 :sample-rate 44100.0
	     :filename "cl-synthesizer-examples/wave-profiling-44100.wav"))
	   (:client-id :wave-writer :init
	    (:duration-seconds 30 :sample-rate 96000.0
	     :filename "cl-synthesizer-examples/wave-profiling-96000.wav")))))

(defparameter *profiling-plan-midi-interface*
  (list
   :name "MIDI-Interface"
   :max-samples 500
   :profile-time t
   :profile-statistical nil
   :init nil
   :jobs '((:client-id :midi-interface :init (:duration-seconds 60)))))


(defparameter *profiling-plan-all*
  (list
   :name "Profile all clients"
   :max-samples 500
   :profile-time t
   :profile-statistical nil
   :init nil
   :jobs '((:client-id :phase-sine-converter :init (:duration-seconds 3600))
	   (:client-id :phase-square-converter :init (:duration-seconds 3600))
	   (:client-id :phase-triangle-converter :init (:duration-seconds 3600))
	   (:client-id :phase-saw-converter :init (:duration-seconds 3600))
	   (:client-id :phase-generator :init (:duration-seconds 3600))
	   (:client-id :rack-core :init (:duration-seconds 60))
	   (:client-id :vco :init (:duration-seconds 60 :vco-count 100))
	   (:client-id :monitor :init (:duration-seconds 120))
	   (:client-id :midi-sequencer :init (:duration-seconds 3600))
	   (:client-id :csv-writer :init (:duration-seconds 60))
	   (:client-id :wave-writer :init (:duration-seconds 60
					   :sample-rate 44100.0
					   :filename "cl-synthesizer-examples/wave-profiling.wav"))
	   (:client-id :midi-interface :init (:duration-seconds 60))
	   )))

;; (run-plan *profiling-plan-vco-core*)
;; (run-plan *profiling-plan-vco*)
;; (run-plan *profiling-plan-rack-core*)
;; (run-plan *profiling-plan-vco-overhead*)
;; (run-plan *profiling-plan-monitor*)
;; (run-plan *profiling-plan-all*)
;; (run-plan *profiling-plan-midi-sequencer*)
;; (run-plan *profiling-plan-csv-writer*)
;; (run-plan *profiling-plan-wave-writer*)
;; (run-plan *profiling-plan-midi-interface*)

