(defpackage :cl-synthesizer-patches-harmonic
  (:use :cl))

(in-package :cl-synthesizer-patches-harmonic)

(defun example (&key (sample-rate 44100))
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment :sample-rate sample-rate)
	       :output-sockets '(:audio))))

    (cl-synthesizer:add-module
     rack "MIXER" #'cl-synthesizer-modules-mixer:make-module
     :channel-count 9 :channel-cv-max 5.0 :channel-cv-gain 5.0 :main-cv-max 5.0 :main-cv-gain 5.0)
    
    (let ((fundamental 440))

      (cl-synthesizer:add-module
       rack "VCO-0"
       #'cl-synthesizer-modules-vco:make-module
       :base-frequency fundamental :v-peak 5.0 :wave-forms '(:sine))
      (cl-synthesizer:add-patch rack "VCO-0" :sine "MIXER" :channel-1)

      (cl-synthesizer:add-module
       rack "VCO-1"
       #'cl-synthesizer-modules-vco:make-module
       :base-frequency (* 2 fundamental) :v-peak 5.0 :wave-forms '(:sine))
      (cl-synthesizer:add-patch rack "VCO-1" :sine "MIXER" :channel-2)

      (cl-synthesizer:add-module
       rack "VCO-2"
       #'cl-synthesizer-modules-vco:make-module
       :base-frequency (* 3 fundamental) :v-peak 5.0 :wave-forms '(:sine))
      (cl-synthesizer:add-patch rack "VCO-2" :sine "MIXER" :channel-3)
      
      (cl-synthesizer:add-module
       rack "VCO-3"
       #'cl-synthesizer-modules-vco:make-module
       :base-frequency (* 4 fundamental) :v-peak 5.0 :wave-forms '(:sine))
      (cl-synthesizer:add-patch rack "VCO-3" :sine "MIXER" :channel-4)

      (cl-synthesizer:add-module
       rack "VCO-4"
       #'cl-synthesizer-modules-vco:make-module
       :base-frequency (* 5 fundamental) :v-peak 5.0 :wave-forms '(:sine))
      (cl-synthesizer:add-patch rack "VCO-4" :sine "MIXER" :channel-5)

      (cl-synthesizer:add-module
       rack "VCO-5"
       #'cl-synthesizer-modules-vco:make-module
       :base-frequency (* 6 fundamental) :v-peak 5.0 :wave-forms '(:sine))
      (cl-synthesizer:add-patch rack "VCO-5" :sine "MIXER" :channel-6)
      
      (cl-synthesizer:add-module
       rack "VCO-6"
       #'cl-synthesizer-modules-vco:make-module
       :base-frequency (* 7 fundamental) :v-peak 5.0 :wave-forms '(:sine))
      (cl-synthesizer:add-patch rack "VCO-6" :sine "MIXER" :channel-7)
      
      (cl-synthesizer:add-module
       rack "VCO-7"
       #'cl-synthesizer-modules-vco:make-module
       :base-frequency (* 8 fundamental) :v-peak 5.0 :wave-forms '(:sine))
      (cl-synthesizer:add-patch rack "VCO-7" :sine "MIXER" :channel-8)

      (cl-synthesizer:add-module
       rack "VCO-8"
       #'cl-synthesizer-modules-vco:make-module
       :base-frequency (* 9 fundamental) :v-peak 5.0 :wave-forms '(:sine))
      (cl-synthesizer:add-patch rack "VCO-8" :sine "MIXER" :channel-9)

      
      (cl-synthesizer:add-patch rack "MIXER" :output "OUTPUT" :audio)

      #|
      (cl-synthesizer-monitor:add-monitor
      rack
      #'cl-synthesizer-monitor-wave-handler:make-handler
      '(("OUTPUT" :input-socket :audio))
      :filename "src/lisp/cl-synthesizer/docs/harmonic.wav")
      |#

      (cl-synthesizer-monitor:add-monitor
       rack
       #'cl-synthesizer-monitor-csv-handler:make-handler
       '(("OUTPUT" :input-socket :audio))
       :filename "src/lisp/cl-synthesizer/docs/harmonic.wav")

      rack)))

(defparameter *duration-seconds* 4.0)
(defparameter *audio-output-sockets* '(:audio))

(defun run-example ()
  (cl-synthesizer:play-rack
   (example)
   :duration-seconds *duration-seconds*))
  
;;(run-example)

