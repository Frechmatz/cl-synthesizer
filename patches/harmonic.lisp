(defpackage :cl-synthesizer-patches-harmonic
  (:use :cl))

(in-package :cl-synthesizer-patches-harmonic)

(defun make-voice (name environment)
  (declare (ignore name))
  (let ((rack (cl-synthesizer:make-rack
	       :environment environment
	       :output-sockets '(:audio))))
    (let ((f-first-harmonic 220.0))
      (let ((waves
	     `((:base-frequency ,f-first-harmonic :v-peak-rel 0.2)
	       (:base-frequency ,(* 2.0 f-first-harmonic) :v-peak-rel 0.37)
	       (:base-frequency ,(* 4.0 f-first-harmonic) :v-peak-rel 1.0)
	       (:base-frequency ,(* 6.0 f-first-harmonic) :v-peak-rel 0.12)
	       (:base-frequency ,(* 8.0 f-first-harmonic) :v-peak-rel 0.15))))
	
	(let ((main-cv-gain 2.5)) ;; war 5.0 (runterdrehen um Clipping zu verhindern)
	  (cl-synthesizer:add-module
	   rack "MIXER" #'cl-synthesizer-modules-mixer:make-module
	   :channel-count (length waves) :channel-cv-max 5.0 :channel-cv-gain 5.0 :main-cv-max 5.0 :main-cv-gain main-cv-gain))
	
	(let ((index 0))
	  (dolist (osc waves)
	    (let ((vco-name (format nil "VCO-~a" index)))
	      (cl-synthesizer:add-module
	       rack vco-name
	       #'cl-synthesizer-modules-vco:make-module
	       :base-frequency (getf osc :base-frequency) :v-peak (* 5.0 (getf osc :v-peak-rel)) :wave-forms '(:sine))
	      (cl-synthesizer:add-patch rack vco-name :sine "MIXER" (cl-synthesizer-macro-util:make-keyword "channel" index))
	      (setf index (+ 1 index))))))
      
      (cl-synthesizer:add-patch rack "MIXER" :output "OUTPUT" :audio)

      rack)))

(defparameter *duration-seconds* 4.0)
(defparameter *audio-output-sockets* '(:audio))

(defun example (&key (sample-rate 44100))
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment :sample-rate sample-rate)
	       :output-sockets *audio-output-sockets*)))

    (cl-synthesizer:add-module rack "VOICE" #'make-voice)

    (cl-synthesizer:add-patch rack "VOICE" :audio "OUTPUT" :audio)

    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:make-handler
     '(("OUTPUT" :input-socket :audio))
     :filename "src/lisp/cl-synthesizer/docs/harmonic.wav")
    
    rack))


(defun run-example ()
  (cl-synthesizer:play-rack
   (example)
   :duration-seconds *duration-seconds*))
  
;;(run-example)

