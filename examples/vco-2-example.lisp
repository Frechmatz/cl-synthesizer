(in-package :cl-synthesizer-examples)


(defun synthesizer-example-vco-2-mono()
  ""
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco-2:vco-2 :footage 440 :cv-max 5 :f-max 8000 :v-peak 5)
    (cl-synthesizer::add-module rack "WAVE-WRITER"
				#'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/out.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :sine "WAVE-WRITER" :channel-1)
    (cl-synthesizer::add-patch rack "WAVE-WRITER" :out-1 "LINE-OUT" :channel-1)
    rack))
  
;;(play-rack (synthesizer-example-vco-2-mono) 5 :attach-speaker t)

(defun synthesizer-example-vco-2-fixed-lin-mono()
  "Expected output frequency is 500 + (10000 / 5) = 2500"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "FIXED" #'cl-synthesizer-modules-fixed-output:fixed-output :value 1)
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco-2:vco-2 :footage 500 :cv-max 5 :f-max 10000 :v-peak 5)
    (cl-synthesizer::add-module rack "WAVE-WRITER"
				#'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/out.wav")
    (cl-synthesizer::add-patch rack "FIXED" :out "VCO-1" :cv-lin)
    (cl-synthesizer::add-patch rack "VCO-1" :sine "WAVE-WRITER" :channel-1)
    (cl-synthesizer::add-patch rack "WAVE-WRITER" :out-1 "LINE-OUT" :channel-1)
    rack))
  
;;(play-rack (synthesizer-example-vco-2-fixed-lin-mono) 5 :attach-speaker t)

