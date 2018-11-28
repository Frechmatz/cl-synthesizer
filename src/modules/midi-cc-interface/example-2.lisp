(defpackage :cl-synthesizer-modules-midi-cc-interface-example-2
  (:use :cl))

(in-package :cl-synthesizer-modules-midi-cc-interface-example-2)

(defparameter *attach-midi* t)
(defparameter *attach-speaker* t)

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
	 (vco-f-max 5000)
	 (vco-cv-max 5)
	 (1Hz (/ vco-cv-max vco-f-max)))
    
    (cl-synthesizer:add-module
     rack "MIDI-CC-IFC" #'cl-synthesizer-modules-midi-cc-interface:make-module
     :controller-numbers (list msb-controller-number lsb-controller-number)
     :initial-output 0.0
     :min-output (* -1 vco-cv-max)
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
     :base-frequency 440
     :cv-max vco-cv-max
     :f-max vco-f-max
     :v-peak 5)

    (cl-synthesizer:add-patch rack "INPUT" :midi-events "MIDI-CC-IFC" :midi-events)
    (cl-synthesizer:add-patch rack "MIDI-CC-IFC" :output "VCO" :cv-lin)
    (cl-synthesizer:add-patch rack "VCO" :sine "OUTPUT" :line-out)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-csv-handler:make-handler
     '(("VCO" :state :frequency :name "Frequency" :format "~,4F")
       ("VCO" :output-socket :sine :name "Sine" :format "~,4F"))
    :filename "waves/midi-cc-interface-example-2.csv"
    :add-header nil)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("VCO" :output-socket :sine))
    :filename "waves/midi-cc-interface-example-2.wav")
    
    rack))

#|
(cl-synthesizer::play-rack (example) 5 
    :attach-audio t :audio-output-sockets '(:line-out) 
    :attach-midi t :midi-input-socket :midi-events)
|#
