(defpackage :cl-synthesizer-monitor-example-1
  (:use :cl))

(in-package :cl-synthesizer-monitor-example-1)

(defun example ()
  "Monitor example."
  (let ((rack (cl-synthesizer:make-rack
	       :environment
	       (cl-synthesizer:make-environment))))

    ;;
    ;; add modules...
    ;;
    
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "ADSR" :input-socket :gate)
       (:channel-2 "ADSR" :output-socket :cv))
     :filename "monitor-example-1.wav")
    
    rack))



