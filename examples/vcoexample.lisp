;;
;; VCO Examples
;;

(in-package :cl-synthesizer-examples)

(defun synthesizer-example-vco-stereo-speaker ()
  "Play two sinus waves in stereo"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco-exponential :base-frequency 440 :f-max 8000 :v-peak 5)
    (cl-synthesizer:add-module rack "VCO-2"
				#'cl-synthesizer-modules-vco:vco-exponential :base-frequency 442 :f-max 8000 :v-peak 5)
    (cl-synthesizer:add-patch rack "VCO-1" :sine "LINE-OUT" :channel-1)
    (cl-synthesizer:add-patch rack "VCO-2" :sine "LINE-OUT" :channel-2)
    
    ;; Add LINE-OUT Monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "LINE-OUT" :input-socket :channel-1) (:channel-2 "LINE-OUT" :input-socket :channel-2))
     :filename "/Users/olli/waves/VCOTwoChannelExample.wav"
     :v-peak 5.0)

    rack))
  
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-stereo-speaker) 5 :attach-speaker t)

(defun synthesizer-example-vco-lfo-stereo-speaker ()
  "Modulate sine with sine-lfo"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "LFO-1"
				#'cl-synthesizer-modules-vco:vco-linear :base-frequency 1.0 :v-peak 1.0 :f-max 500 :cv-max 5)
    (cl-synthesizer:add-module rack "LFO-2"
				#'cl-synthesizer-modules-vco:vco-linear :base-frequency 1.0 :v-peak 1.0 :f-max 500 :cv-max 5)
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco-linear :base-frequency 440 :f-max 5000 :v-peak 5 :cv-max 5)
    (cl-synthesizer:add-module rack "VCO-2"
				#'cl-synthesizer-modules-vco:vco-linear :base-frequency 440 :f-max 5000 :v-peak 5 :cv-max 5)
    
    (cl-synthesizer:add-patch rack "LFO-1" :sine "VCO-1" :cv)
    (cl-synthesizer:add-patch rack "LFO-2" :sine "VCO-2" :cv)
    (cl-synthesizer:add-patch rack "VCO-1" :sine "LINE-OUT" :channel-1)
    (cl-synthesizer:add-patch rack "VCO-2" :sine "LINE-OUT" :channel-2)

    ;; Add Oscilloscope Monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "LFO-1" :output-socket :sine)
       (:channel-2 "LFO-2" :output-socket :sine)
       (:channel-3 "VCO-1" :output-socket :sine)
       (:channel-4 "VCO-2" :output-socket :sine))
     :filename "/Users/olli/waves/VCOFourChannelExample.wav")
    
    ;; Add LINE-OUT Monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "LINE-OUT" :input-socket :channel-1) (:channel-2 "LINE-OUT" :input-socket :channel-2))
     :filename "/Users/olli/waves/AudioOut.wav")
    
    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-lfo-stereo-speaker) 10 :attach-speaker t)

