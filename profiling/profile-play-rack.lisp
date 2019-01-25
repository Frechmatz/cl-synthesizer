;;
;; Profile cl-synthesizer::play-rack
;;

(defpackage :cl-synthesizer-profiling-play-rack
  (:use :cl))

(in-package :cl-synthesizer-profiling-play-rack)

(defun mock-input-device (name environment)
  (declare (ignore name environment))
    (list
     :outputs (lambda() '(:output-1 :output-2))
     :inputs (lambda() '(:input-1 :input-2))
     :update (lambda () nil)
     :get-output (lambda (output)
		   (declare (ignore output))
		   1.0)
     :shutdown (lambda() nil)))

(defun mock-output-device (name environment)
  (declare (ignore name environment))
  (list
   :outputs (lambda() '(:output-1 :output-2))
   :inputs (lambda() '(:input-1 :input-2))
   :update (lambda (input-args) (declare (ignore input-args)) nil)
   :get-output (lambda (output)
		 (declare (ignore output))
		 1.0)
   :shutdown (lambda() nil)))

(defparameter *input-device-settings*
  (list
   :symbol-name "MOCK-INPUT-DEVICE"
   :package-name "CL-SYNTHESIZER-PROFILING-PLAY-RACK"))

(defparameter *output-device-settings*
  (list
   :symbol-name "MOCK-OUTPUT-DEVICE"
   :package-name "CL-SYNTHESIZER-PROFILING-PLAY-RACK"))

(defun prepare (&key duration-seconds attach-input-device attach-output-device)
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment)
	       :input-sockets '(:input-1 :input-2)
	       :output-sockets '(:output-1 :output-2))))
    (lambda()
      (let ((cl-synthesizer::*audio-device-settings* *output-device-settings*)
	    (cl-synthesizer::*midi-device-settings* *input-device-settings*))
	(cl-synthesizer::play-rack
	 rack duration-seconds
	 :attach-audio attach-output-device :audio-output-sockets '(:output-1 :output-2) 
	 :attach-midi attach-input-device :midi-input-socket :input-1)))))

