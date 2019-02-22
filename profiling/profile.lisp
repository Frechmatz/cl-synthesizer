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

;;
;; Client repository (The code to be profiled)
;;

(defparameter *clients*
  (list
   (list
    :id :rack-core-nested :name "Rack Core: Nested and patched modules"
    :setup (lambda(&key duration-seconds)
	     (let ((main-module-count 4) (sub-module-count 4) (module-io-socket-count 4))
	       (let ((rack (cl-synthesizer-profiling-rack::make-test-rack-nested
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
    :id :rack-core-flat :name "Rack Core: Flat modules without patches"
    :setup (lambda(&key duration-seconds module-count input-socket-count output-socket-count)
	       (let ((rack (cl-synthesizer-profiling-rack::make-test-rack-flat
			    :module-count module-count :input-socket-count input-socket-count
			    :output-socket-count output-socket-count)))
		 (let ((info (get-rack-info rack)))
		   (values 
		    (lambda ()
		      (cl-synthesizer:play-rack
		       rack
		       :duration-seconds duration-seconds))
		    (format
		     nil
		     "Updating ~a dummy modules for ~a seconds (Modules: ~a Input sockets: ~a Output sockets: ~a Patches: ~a)"
		     module-count duration-seconds (getf info :module-count) input-socket-count output-socket-count
		     (getf info :patch-count))
		    )))))
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
		(format nil "Converting phase ~,2F to sine waveform for ~a seconds" phi duration-seconds)))))
   (list
    :id :phase-square-converter :name "Phase-Square Converter"
    :setup (lambda(&key duration-seconds)
	     (let ((phi 10.0))
	       (values 
		(lambda()
		  (cl-synthesizer-profiling-waveform-converter::run-square
		   :duration-seconds duration-seconds :phi phi))
		(format nil "Converting phase ~,2F to square waveform for ~a seconds" phi duration-seconds)))))
   (list
    :id :phase-triangle-converter :name "Phase-Triangle Converter"
    :setup (lambda(&key duration-seconds)
	     (let ((phi 10.0))
	       (values 
		(lambda()
		  (cl-synthesizer-profiling-waveform-converter::run-triangle
		   :duration-seconds duration-seconds :phi phi))
		(format nil "Converting phase ~,2F to triangle waveform for ~a seconds" phi duration-seconds)))))
   (list
    :id :phase-saw-converter :name "Phase-Saw Converter"
    :setup (lambda(&key duration-seconds)
	     (let ((phi 10.0))
	       (values 
		(lambda()
		  (cl-synthesizer-profiling-waveform-converter::run-saw
		   :duration-seconds duration-seconds :phi phi))
		(format nil "Converting phase ~,2F to saw waveform for ~a seconds" phi duration-seconds)))))
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
    :setup (lambda(&key duration-seconds sample-rate filename)
	     (let ((rack (cl-synthesizer-profiling-csv-file-writer::make-test-rack
			  :sample-rate sample-rate
			  :filename filename)))
	       (values 
		(lambda ()
		  (cl-synthesizer:play-rack rack :duration-seconds duration-seconds))
		(format
		 nil
		 "Updating CSV-Writer for ~a seconds with sample-rate ~a into file ~a"
		 duration-seconds sample-rate filename)))))
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

   (list
    :id :adsr :name "ADSR"
    :setup (lambda(&key duration-seconds)
	     (let ((rack (cl-synthesizer-profiling-adsr::make-test-rack)))
	       (values 
		(lambda ()
		  (cl-synthesizer:play-rack
		       rack
		       :duration-seconds duration-seconds))
		(format nil "Updating ADSR for ~a seconds" duration-seconds)
		))))
   (list
    :id :mixer :name "Mixer"
    :setup (lambda(&key duration-seconds channel-count)
	     (let ((rack (cl-synthesizer-profiling-mixer::make-test-rack :channel-count channel-count)))
	       (let ((info (get-rack-info rack)))
		 (values 
		  (lambda ()
		    (cl-synthesizer:play-rack
		     rack
		     :duration-seconds duration-seconds))
		  (format nil "Updating Mixer with ~a channels for ~a seconds (Modules: ~a Patches: ~a)"
			  channel-count duration-seconds
			  (getf info :module-count) (getf info :patch-count)))
		))))

   ))

;;
;; Profiling plan runner
;;

(defvar *report* nil)

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
  (let ((start (get-internal-real-time)))
    (time (funcall fn))
    (let ((end (get-internal-real-time)))
      (push (list :elapsed-time (/ (- end start) INTERNAL-TIME-UNITS-PER-SECOND)) *report*)))
  nil)

(defun print-report (profiling-plan-name)
  (format t "Report: '~a':~%Elapsed time,Job" profiling-plan-name)
  (let ((cur-report nil))
    (flet ((flush-report ()
	     (if cur-report
		 (progn
		   (format t "~%~a ~a" (getf cur-report :elapsed-time) (getf cur-report :name))
		   (setf cur-report nil)
		   ))))
      (dolist (report-entry (reverse *report*))
	(cond 
	  ((eq :start (first report-entry))
	   (flush-report)
	   (setf (getf cur-report :name) (second report-entry)))
	  ((eq :elapsed-time (first report-entry))
	   (setf (getf cur-report :elapsed-time) (format nil "~,2Fs" (second report-entry))))
	  (t
	   (format t "~%Unsupported report entry: ~a~%" report-entry))
	  ))
      (flush-report)))
      (format t "~%"))
  
(defun run-profiler (client client-lambdalist profiler-settings profiler-name profiler-fn)
  (let ((job-name (getf client :name)))
    (multiple-value-bind (fn description)
	(apply (getf client :setup) client-lambdalist)
      (format t "~%Running client '~a' with profiler ~a...~%==> ~a~%" job-name profiler-name description)
      (push (list :start description) *report*)
      (funcall profiler-fn fn profiler-settings)
      (format t "~%Client '~a' with profiler ~a has completed~%" job-name profiler-name))))

(defun run-plan (profiling-plan)
  (format t "~%Running plan '~a'~%" (getf profiling-plan :name))
  (let ((*report* nil))
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
    (print-report (getf profiling-plan :name))
    (format t "~%Plan '~a' has completed~%" (getf profiling-plan :name)))
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
   :jobs '((:client-id :rack-core-nested
	    :init (:duration-seconds 60))
	   (:client-id :rack-core-flat
	    :init (:duration-seconds 60 :module-count 100 :input-socket-count 3 :output-socket-count 4)))))

(defparameter *profiling-plan-vco-overhead*
  (list
   :name "Measure overhead of 100 VCO Modules against 100 dummy modules and core calls"
   :max-samples 500
   :profile-time t
   :profile-statistical nil
   :init nil
   :jobs '((:client-id :vco
	    :init (:duration-seconds 60 :vco-count 100))
	   ;; A VCO has 2 input and 4 output sockets
	   (:client-id :rack-core-flat
	    :init (:duration-seconds 60 :module-count 100 :input-socket-count 2 :output-socket-count 4))
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
   :jobs '((:client-id :csv-writer :init
	    (:duration-seconds 60
	     :filename "cl-synthesizer-examples/csv-profiling.csv"
	     :sample-rate 44100
	     )))))

(defparameter *profiling-plan-wave-writer*
  (list
   :name "Wave-Writer"
   :max-samples 500
   :profile-time t
   :profile-statistical nil
   :init nil
   :jobs '(
	   (:client-id :wave-writer :init
	    (:duration-seconds 30 :sample-rate 44100
	     :filename "cl-synthesizer-examples/wave-profiling-44100.wav"))
	   (:client-id :wave-writer :init
	    (:duration-seconds 30 :sample-rate 96000
	     :filename "cl-synthesizer-examples/wave-profiling-96000.wav")))))

(defparameter *profiling-plan-midi-interface*
  (list
   :name "MIDI-Interface"
   :max-samples 500
   :profile-time t
   :profile-statistical nil
   :init nil
   :jobs '((:client-id :midi-interface :init (:duration-seconds 60)))))

(defparameter *profiling-plan-adsr*
  (list
   :name "ADSR"
   :max-samples 500
   :profile-time t
   :profile-statistical nil
   :init nil
   :jobs '((:client-id :adsr :init (:duration-seconds 60)))))

(defparameter *profiling-plan-mixer*
  (list
   :name "Mixer"
   :max-samples 500
   :profile-time t
   :profile-statistical nil
   :init nil
   :jobs '((:client-id :mixer :init (:duration-seconds 60 :channel-count 32)))))


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
	   (:client-id :rack-core-nested :init (:duration-seconds 60))
	   (:client-id :rack-core-flat :init
	    (:duration-seconds 60
	     :module-count 100
	     :input-socket-count 3
	     :output-socket-count 4))
	   (:client-id :vco :init (:duration-seconds 60 :vco-count 100))
	   (:client-id :monitor :init (:duration-seconds 120))
	   (:client-id :midi-sequencer :init (:duration-seconds 3600))
	   (:client-id :csv-writer :init
	    (:duration-seconds 60
	     :sample-rate 44100
	     :filename "cl-synthesizer-examples/csv-profiling.csv"))
	   (:client-id :wave-writer :init
	    (:duration-seconds 60
	     :sample-rate 44100
	     :filename "cl-synthesizer-examples/wave-profiling.wav"))
	   (:client-id :midi-interface :init (:duration-seconds 60))
	   (:client-id :adsr :init (:duration-seconds 60))
	   (:client-id :mixer :init (:duration-seconds 60 :channel-count 32))
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
;; (run-plan *profiling-plan-adsr*)
;; (run-plan *profiling-plan-mixer*)

