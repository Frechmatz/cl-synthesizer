;;
;; VCO Examples
;;

(in-package :cl-synthesizer-examples)

(defun synthesizer-example-vco-stereo-speaker ()
  "Play two sinus waves in stereo"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :footage 440 :cv-max 5 :f-max 8000 :v-peak 5)
    (cl-synthesizer::add-module rack "VCO-2"
				#'cl-synthesizer-modules-vco:vco :footage 442 :cv-max 5 :f-max 8000 :v-peak 5)
    (cl-synthesizer::add-module rack "WAVE-WRITER"
				#'cl-synthesizer-modules-wave-file-writer::two-channel-wave-file-writer
				:filename "/Users/olli/waves/VCOTwoChannelExample.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :sine "WAVE-WRITER" :channel-1)
    (cl-synthesizer::add-patch rack "VCO-2" :sine "WAVE-WRITER" :channel-2)
    (cl-synthesizer::add-patch rack "WAVE-WRITER" :out-1 "LINE-OUT" :channel-1)
    (cl-synthesizer::add-patch rack "WAVE-WRITER" :out-2 "LINE-OUT" :channel-2)
    rack))
  
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-stereo-speaker) 5 :attach-speaker t)

(defun synthesizer-example-vco-lfo-stereo-speaker ()
  "Modulate sine with sine-lfo"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "LFO-1"
				#'cl-synthesizer-modules-vco::vco :footage 0.2 :v-peak 5)
    (cl-synthesizer::add-module rack "LFO-2"
				#'cl-synthesizer-modules-vco::vco :footage 0.2 :v-peak 5)
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :footage 440 :cv-max 5 :f-max 8000 :v-peak 5)
    (cl-synthesizer::add-module rack "VCO-2"
				#'cl-synthesizer-modules-vco:vco :footage 440 :cv-max 5 :f-max 8000 :v-peak 5)
    
    (cl-synthesizer::add-module rack "WAVE-WRITER-AUDIO"
				#'cl-synthesizer-modules-wave-file-writer::two-channel-wave-file-writer
				:filename "/Users/olli/waves/AudioOut.wav")
    (cl-synthesizer::add-module rack "WAVE-WRITER-OSCILLOSCOPE"
				#'cl-synthesizer-modules-wave-file-writer::four-channel-wave-file-writer
				:filename "/Users/olli/waves/VCOFourChannelExample.wav")

    (cl-synthesizer::add-patch rack "VCO-1" :sine "WAVE-WRITER-OSCILLOSCOPE" :channel-1)
    (cl-synthesizer::add-patch rack "VCO-2" :sine "WAVE-WRITER-OSCILLOSCOPE" :channel-2)
    (cl-synthesizer::add-patch rack "LFO-1" :sine "WAVE-WRITER-OSCILLOSCOPE" :channel-3)
    (cl-synthesizer::add-patch rack "WAVE-WRITER-OSCILLOSCOPE" :out-3 "VCO-1" :cv-lin)
    (cl-synthesizer::add-patch rack "LFO-2" :sine "WAVE-WRITER-OSCILLOSCOPE" :channel-4)
    (cl-synthesizer::add-patch rack "WAVE-WRITER-OSCILLOSCOPE" :out-4 "VCO-2" :cv-lin)

    (cl-synthesizer::add-patch rack "WAVE-WRITER-OSCILLOSCOPE" :out-1 "WAVE-WRITER-AUDIO" :channel-1)
    (cl-synthesizer::add-patch rack "WAVE-WRITER-OSCILLOSCOPE" :out-2 "WAVE-WRITER-AUDIO" :channel-2)
    (cl-synthesizer::add-patch rack "WAVE-WRITER-AUDIO" :out-1 "LINE-OUT" :channel-1)
    (cl-synthesizer::add-patch rack "WAVE-WRITER-AUDIO" :out-2 "LINE-OUT" :channel-2)
    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-lfo-stereo-speaker) 5 :attach-speaker t)

(defun synthesizer-example-vco-triangle ()
  "Triangle test"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :footage 300 :cv-max 5 :f-max 8000 :v-peak 5)
    (cl-synthesizer::add-module rack "WAVE-WRITER-OSCILLOSCOPE"
				#'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/triangle.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :triangle "WAVE-WRITER-OSCILLOSCOPE" :channel-1)
    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-triangle) 3)

(defun synthesizer-example-vco-triangle-sweep ()
  "Triangle test"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "LFO-1"
				#'cl-synthesizer-modules-vco::vco :footage 0.5 :v-peak 5)
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :footage 400 :cv-max 5 :f-max 660 :v-peak 5)
    (cl-synthesizer::add-module rack "WAVE-WRITER-OSCILLOSCOPE"
				#'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/triangle.wav")

    (cl-synthesizer::add-patch rack "LFO-1" :triangle "VCO-1" :cv)
    (cl-synthesizer::add-patch rack "VCO-1" :triangle "WAVE-WRITER-OSCILLOSCOPE" :channel-1)
    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-triangle-sweep) 3)

(defun synthesizer-example-vco-saw ()
  "Saw test"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :footage 300 :cv-max 5 :f-max 8000 :v-peak 5)
    (cl-synthesizer::add-module rack "WAVE-WRITER-OSCILLOSCOPE"
				#'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/saw.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :saw "WAVE-WRITER-OSCILLOSCOPE" :channel-1)
    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-saw) 3)


(defun synthesizer-example-vco-saw-sweep ()
  "Saw test"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "LFO-1"
				#'cl-synthesizer-modules-vco::vco :footage 0.5 :v-peak 5)
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :footage 440 :cv-max 5 :f-max 660 :v-peak 5)
    (cl-synthesizer::add-module rack "WAVE-WRITER-OSCILLOSCOPE"
				#'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/sawsweep.wav")

    (cl-synthesizer::add-patch rack "LFO-1" :triangle "VCO-1" :cv-lin)
    (cl-synthesizer::add-patch rack "VCO-1" :saw "WAVE-WRITER-OSCILLOSCOPE" :channel-1)
    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-saw-sweep) 3)


(defun synthesizer-example-vco-square ()
  "Saw test"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :footage 300 :cv-max 5 :v-peak 5)
    (cl-synthesizer::add-module rack "WAVE-WRITER-OSCILLOSCOPE"
				#'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/square.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :square "WAVE-WRITER-OSCILLOSCOPE" :channel-1)
    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-square) 3)

(defun synthesizer-example-vco-square-sweep ()
  "Saw test"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "LFO-1"
				#'cl-synthesizer-modules-vco::vco :footage 0.5 :v-peak 5)
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :footage 440 :cv-max 5 :v-peak 5)
    (cl-synthesizer::add-module rack "WAVE-WRITER-OSCILLOSCOPE"
				#'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/squaresweep.wav")

    (cl-synthesizer::add-patch rack "LFO-1" :triangle "VCO-1" :cv-lin)
    (cl-synthesizer::add-patch rack "VCO-1" :square "WAVE-WRITER-OSCILLOSCOPE" :channel-1)
    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-square-sweep) 3)

(defun synthesizer-example-vco-all-waves ()
  "Saw test"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :footage 300 :cv-max 5 :v-peak 5)
    (cl-synthesizer::add-module rack "WAVE-WRITER-OSCILLOSCOPE"
				#'cl-synthesizer-modules-wave-file-writer::four-channel-wave-file-writer
				:filename "/Users/olli/waves/vcoallwaves.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :sine "WAVE-WRITER-OSCILLOSCOPE" :channel-1)
    (cl-synthesizer::add-patch rack "VCO-1" :triangle "WAVE-WRITER-OSCILLOSCOPE" :channel-2)
    (cl-synthesizer::add-patch rack "VCO-1" :saw "WAVE-WRITER-OSCILLOSCOPE" :channel-3)
    (cl-synthesizer::add-patch rack "VCO-1" :square "WAVE-WRITER-OSCILLOSCOPE" :channel-4)
    rack))
      
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-all-waves) 3)

(defun synthesizer-example-vco-2-mono()
  "TODO: Is this example redundant?"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :footage 440 :cv-max 5 :f-max 8000 :v-peak 5)
    (cl-synthesizer::add-module rack "WAVE-WRITER"
				#'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/out.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :sine "WAVE-WRITER" :channel-1)
    (cl-synthesizer::add-patch rack "WAVE-WRITER" :out-1 "LINE-OUT" :channel-1)
    rack))
  
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-2-mono) 5 :attach-speaker t)

(defun synthesizer-example-vco-2-fixed-lin-mono()
  "Expected output frequency is 500 + (10000 / 5) = 2500"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "FIXED" #'cl-synthesizer-modules-fixed-output:fixed-output :value 1)
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :footage 500 :cv-max 5 :f-max 10000 :v-peak 5)
    (cl-synthesizer::add-module rack "WAVE-WRITER"
				#'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/out.wav")
    (cl-synthesizer::add-patch rack "FIXED" :out "VCO-1" :cv-lin)
    (cl-synthesizer::add-patch rack "VCO-1" :sine "WAVE-WRITER" :channel-1)
    (cl-synthesizer::add-patch rack "WAVE-WRITER" :out-1 "LINE-OUT" :channel-1)
    rack))
  
;;(cl-synthesizer-util:play-rack (synthesizer-example-vco-2-fixed-lin-mono) 5 :attach-speaker t)

