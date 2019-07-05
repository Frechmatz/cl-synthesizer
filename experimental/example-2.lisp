(defpackage :cl-synthesizer-experimental-example-2
  (:use :cl))

(in-package :cl-synthesizer-experimental-example-2)

(defun example ()
  "Modulate frequency via two CC controllers"
  (let* ((rack (cl-synthesizer:make-rack
		:environment (cl-synthesizer:make-environment)
		:input-sockets '(:midi-events)
		:output-sockets '(:line-out)))
	 (lsb-controller-number
	  (funcall
	   (getf cl-synthesizer-vendor:*arturia-minilab-mk2* :get-controller-number) :encoder-1))
	 (msb-controller-number
	  (funcall
	   (getf cl-synthesizer-vendor:*arturia-minilab-mk2* :get-controller-number) :encoder-9))
	 (delta-1-hz 0.01))
    
    (cl-synthesizer:add-module
     rack "MIDI-CC-IFC" #'cl-synthesizer-modules-midi-cc-interface:make-module
     :controller-numbers (list msb-controller-number lsb-controller-number)
     :initial-output 0.0
     :min-output (* -5000.0 delta-1-hz)
     :max-output (* 5000.0 delta-1-hz)
     :channel nil
     :transform-handler
     (lambda (cur-output controller-number control-value)
       (let ((offs 
	      (cond
		((= 61 control-value) (* -1.0 delta-1-hz))
		((= 62 control-value) (* -2.0 delta-1-hz))
		((= 63 control-value) (* -3.0 delta-1-hz))
		((= 65 control-value) (* 1.0  delta-1-hz))
		((= 66 control-value) (* 2.0 delta-1-hz))
		((= 67 control-value) (* 3.0 delta-1-hz))
		(t 0))))
	 (if (= controller-number msb-controller-number)
	     (setf offs (* 10.0 offs))) ;; Jump in 10Hz steps
	 (+ cur-output offs))))
    
    (cl-synthesizer:add-module
     rack "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 440.0
     :cv-lin-hz-v (/ 1.0 delta-1-hz)
     :v-peak 5.0)

    (cl-synthesizer:add-patch rack "INPUT" :midi-events "MIDI-CC-IFC" :midi-events)
    (cl-synthesizer:add-patch rack "MIDI-CC-IFC" :output "VCO" :cv-lin)
    (cl-synthesizer:add-patch rack "VCO" :sine "OUTPUT" :line-out)

    rack))

(defun run-example ()
  (cl-synthesizer-experimental::play-rack
   (example)
   :duration-seconds 10
   :attach-audio t
   :audio-output-sockets '(:line-out) 
   :attach-midi t :midi-input-socket :midi-events))


;; (run-example)
