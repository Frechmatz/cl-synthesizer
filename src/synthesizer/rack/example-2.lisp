(defpackage :cl-synthesizer-rack-example-2
  (:use :cl))

(in-package :cl-synthesizer-rack-example-2)

(defparameter *attach-speaker* nil)


(defun make-voice (name environment &key lfo-frequency vco-frequency)
  (declare (ignore name))
  (let ((rack (cl-synthesizer:make-rack
	       :environment environment
	       :input-sockets nil
	       :output-sockets '(:audio))))
    
    (cl-synthesizer:add-module
     rack "LFO"
     #'cl-synthesizer-modules-vco:vco-linear
     :base-frequency lfo-frequency :v-peak 1.0 :f-max 500 :cv-max 5)
    
    (cl-synthesizer:add-module
     rack "VCO"
     #'cl-synthesizer-modules-vco:vco-linear
     :base-frequency vco-frequency :f-max 5000 :v-peak 5 :cv-max 5)
    
    (cl-synthesizer:add-patch rack "LFO" :sine "VCO" :cv)
    (cl-synthesizer:add-patch rack "VCO" :saw "OUTPUT" :audio)
    
    rack))

(defun example ()
  "Modulate the frequency of a saw signal with a LFO."
  (let ((rack (cl-synthesizer:make-rack
	       :environment
	       (cl-synthesizer:make-environment))))

    (cl-synthesizer:add-module
     rack "VOICE-1" #'make-voice :lfo-frequency 1.0 :vco-frequency 440)
    (cl-synthesizer:add-module
     rack "VOICE-2" #'make-voice :lfo-frequency 2.0 :vco-frequency 442)
    
    (cl-synthesizer:add-patch rack "VOICE-1" :audio "LINE-OUT" :channel-1)
    (cl-synthesizer:add-patch rack "VOICE-2" :audio "LINE-OUT" :channel-2)

    ;; Write LINE-OUT to Wave-File
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "LINE-OUT" :input-socket :channel-1)
       (:channel-2 "LINE-OUT" :input-socket :channel-2))
     :filename "rack-example-2.wav")
    
    rack))

;;(cl-synthesizer:play-rack (example) 5 :attach-speaker *attach-speaker*)


