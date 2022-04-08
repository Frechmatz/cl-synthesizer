(defpackage :cl-synthesizer-profiling-mixer
  (:use :cl))

(in-package :cl-synthesizer-profiling-mixer)

(defun setup-channel (rack channel-number)
  (let ((fixed-output-module-name (format nil "CH-~a" channel-number)))
    (cl-synthesizer:add-module
     rack
     fixed-output-module-name
     #'cl-synthesizer-modules-fixed-output:make-module
     :value 1.0)
    (cl-synthesizer:add-patch
     rack
     fixed-output-module-name :out
     "MIXER" (cl-synthesizer-lisp-util:make-keyword "CHANNEL" (+ channel-number 1)))))

    
(defun make-test-rack (&key channel-count)
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "MIXER"
			       #'cl-synthesizer-modules-mixer:make-module
			       :channel-count channel-count
			       :channel-cv-max 5.0
			       :channel-cv-gain 5.0
			       :main-cv-max 5.0
			       :main-cv-gain 5.0)
    (dotimes (i channel-count)
      (setup-channel rack i))
    rack))

