
(in-package :cl-synthesizer-examples)


(defun synthesizer-example-440hz-two-channel ()
  "Write two 440Hz channels"
  (let ((rack (make-instance 'cl-synthesizer:rack)))
    (cl-synthesizer::add-module rack "VCO-1" #'cl-synthesizer-modules::cosinus-vco :f_0 440)
    (cl-synthesizer::add-module rack "VCO-2" #'cl-synthesizer-modules::cosinus-vco :f_0 440)
    (cl-synthesizer::add-module rack "WAVE-WRITER"
				#'cl-synthesizer-modules::two-channel-wave-file-writer
				:filename "/Users/olli/waves/440HzTwoChannelExample.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :out "WAVE-WRITER" :channel-1)
    (cl-synthesizer::add-patch rack "VCO-2" :out "WAVE-WRITER" :channel-2)
    (dotimes (i 100000)
      (cl-synthesizer::update-rack rack))
    (cl-synthesizer::shutdown-rack rack)))

;; (synthesizer-example-440hz-two-channel)


(defun synthesizer-example-440hz-one-channel ()
  "Write one 440Hz channel"
  (let ((rack (make-instance 'cl-synthesizer:rack)))
    (cl-synthesizer::add-module rack "VCO-1" #'cl-synthesizer-modules::cosinus-vco :f_0 440)
    (cl-synthesizer::add-module rack "WAVE-WRITER" #'cl-synthesizer-modules::one-channel-wave-file-writer
				:filename "/Users/olli/waves/440HzOneChannelExample.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :out "WAVE-WRITER" :channel-1)
    (dotimes (i 100000)
      (cl-synthesizer::update-rack rack))
    (cl-synthesizer::shutdown-rack rack)))

;; (synthesizer-example-440hz-one-channel)
