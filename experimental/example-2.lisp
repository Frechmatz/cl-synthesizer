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
	 (vco-f-max 5000.0)
	 (vco-cv-max 5.0)
	 (1Hz (/ vco-cv-max vco-f-max)))
    
    (cl-synthesizer:add-module
     rack "MIDI-CC-IFC" #'cl-synthesizer-modules-midi-cc-interface:make-module
     :controller-numbers (list msb-controller-number lsb-controller-number)
     :initial-output 0.0
     :min-output (* -1.0 vco-cv-max)
     :max-output vco-cv-max
     :channel nil
     :transform-handler
     (lambda (cur-output controller-number control-value)
       (let ((offs 
	      (cond
		((= 61 control-value) (* -1.0 1Hz))
		((= 62 control-value) (* -2.0 1Hz))
		((= 63 control-value) (* -3.0 1Hz))
		((= 65 control-value) (* 1.0 1Hz))
		((= 66 control-value) (* 2.0 1Hz))
		((= 67 control-value) (* 3.0 1Hz))
		(t 0))))
	 (if (= controller-number msb-controller-number)
	     (setf offs (* 10.0 offs))) 
	 (+ cur-output offs))))
    
    (cl-synthesizer:add-module
     rack "VCO"
     #'cl-synthesizer-modules-vco:make-module
     :base-frequency 440.0
     :cv-max vco-cv-max
     :f-max vco-f-max
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
