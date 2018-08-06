;;
;; VCO Examples
;;

(in-package :cl-synthesizer-examples)

(defun synthesizer-example-vco-stereo-speaker ()
  "Play two sinus waves in stereo"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :base-frequency 440 :cv-linear-max 5 :f-max 8000 :v-peak 5)
    (cl-synthesizer:add-module rack "VCO-2"
				#'cl-synthesizer-modules-vco:vco :base-frequency 442 :cv-linear-max 5 :f-max 8000 :v-peak 5)
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
				#'cl-synthesizer-modules-vco:vco :base-frequency 0.2 :v-peak 5)
    (cl-synthesizer:add-module rack "LFO-2"
				#'cl-synthesizer-modules-vco:vco :base-frequency 0.2 :v-peak 5)
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :base-frequency 440 :cv-linear-max 5 :f-max 8000 :v-peak 5)
    (cl-synthesizer:add-module rack "VCO-2"
				#'cl-synthesizer-modules-vco:vco :base-frequency 440 :cv-linear-max 5 :f-max 8000 :v-peak 5)
    
    (cl-synthesizer:add-patch rack "LFO-1" :sine "VCO-1" :cv-linear)
    (cl-synthesizer:add-patch rack "LFO-2" :sine "VCO-2" :cv-linear)
    (cl-synthesizer:add-patch rack "VCO-1" :sine "LINE-OUT" :channel-1)
    (cl-synthesizer:add-patch rack "VCO-2" :sine "LINE-OUT" :channel-2)

    ;; Add Oscilloscope Monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "VCO-1" :output-socket :sine)
       (:channel-2 "VCO-2" :output-socket :sine)
       (:channel-3 "LFO-1" :output-socket :sine)
       (:channel-4 "LFO-2" :output-socket :sine))
     :filename "/Users/olli/waves/VCOFourChannelExample.wav")
    
    ;; Add LINE-OUT Monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "LINE-OUT" :input-socket :channel-1) (:channel-2 "LINE-OUT" :input-socket :channel-2))
     :filename "/Users/olli/waves/AudioOut.wav")
    
    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-lfo-stereo-speaker) 5 :attach-speaker t)

(defun synthesizer-example-vco-triangle-sweep ()
  "Triangle test"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "LFO-1"
				#'cl-synthesizer-modules-vco:vco :base-frequency 0.5 :v-peak 5)
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :base-frequency 400 :cv-linear-max 5 :f-max 660 :v-peak 5)
    (cl-synthesizer:add-patch rack "LFO-1" :triangle "VCO-1" :cv)

    ;; Add Oscilloscope Monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "VCO-1" :output-socket :triangle))
     :filename "/Users/olli/waves/triangle.wav")

    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-triangle-sweep) 3)

(defun synthesizer-example-vco-saw ()
  "Saw test"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :base-frequency 300 :cv-linear-max 5 :f-max 8000 :v-peak 5)

    ;; Add Oscilloscope Monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "VCO-1" :output-socket :saw))
     :filename "/Users/olli/waves/saw.wav")

    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-saw) 3)


(defun synthesizer-example-vco-saw-sweep ()
  "Saw test"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "LFO-1"
				#'cl-synthesizer-modules-vco:vco :base-frequency 0.5 :v-peak 5)
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :base-frequency 440 :cv-linear-max 5 :f-max 660 :v-peak 5)
    (cl-synthesizer:add-patch rack "LFO-1" :triangle "VCO-1" :cv-linear)

    ;; Add Oscilloscope Monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "VCO-1" :output-socket :saw))
     :filename "/Users/olli/waves/sawsweep.wav")

    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-saw-sweep) 3)


(defun synthesizer-example-vco-square ()
  "Saw test"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :base-frequency 300 :cv-linear-max 5 :v-peak 5)
    ;; Add Oscilloscope Monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "VCO-1" :output-socket :square))
     :filename "/Users/olli/waves/square.wav")

    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-square) 3)

(defun synthesizer-example-vco-square-sweep ()
  "Saw test"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "LFO-1"
				#'cl-synthesizer-modules-vco:vco :base-frequency 0.5 :v-peak 5)
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :base-frequency 440 :cv-linear-max 5 :v-peak 5)

    (cl-synthesizer:add-patch rack "LFO-1" :triangle "VCO-1" :cv-linear)

    ;; Add Oscilloscope Monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "VCO-1" :output-socket :square))
     :filename "/Users/olli/waves/squaresweep.wav")

    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-square-sweep) 3)

(defun synthesizer-example-vco-all-waves ()
  "Saw test"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :base-frequency 300 :cv-linear-max 5 :v-peak 5)

    ;; Add Oscilloscope Monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "VCO-1" :output-socket :sine)
       (:channel-2 "VCO-1" :output-socket :triangle)
       (:channel-3 "VCO-1" :output-socket :saw)
       (:channel-4 "VCO-1" :output-socket :square))
     :filename "/Users/olli/waves/allwaves.wav")

    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-all-waves) 3)

(defun synthesizer-example-vco-2-fixed-lin-mono()
  "Expected output frequency is 500 + (10000 / 5) = 2500"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "FIXED" #'cl-synthesizer-modules-fixed-output:fixed-output :value 1)
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :base-frequency 500 :cv-linear-max 5 :f-max 10000 :v-peak 5)
    (cl-synthesizer:add-patch rack "FIXED" :out "VCO-1" :cv-linear)

    (cl-synthesizer:add-patch rack "VCO-1" :sine "LINE-OUT" :channel-1)
    
    ;; Add Oscilloscope Monitor
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 "VCO-1" :output-socket :sine))
     :filename "/Users/olli/waves/out.wav")

    rack))
  
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-2-fixed-lin-mono) 5 :attach-speaker t)

