
(in-package :cl-synthesizer-examples)


(defun synthesizer-example-440hz ()
  "Write two 440Hz channels into 440HzExample.wav"
  (let ((rack (make-instance 'cl-synthesizer:rack)))
    (cl-synthesizer::add-module rack (cl-synthesizer::make-rack-module
				      "VCO-1"
				      #'cl-synthesizer-modules::cosinus-vco
				      :f_0 440))
    (cl-synthesizer::add-module rack (cl-synthesizer::make-rack-module
				      "VCO-2"
				      #'cl-synthesizer-modules::cosinus-vco
				      :f_0 440))
    (cl-synthesizer::add-module rack (cl-synthesizer::make-rack-module
				      "WAVE-WRITER"
				      #'cl-synthesizer-modules::two-channel-wave-file-writer
				      :filename "/Users/olli/440HzExample.wav"))
    (cl-synthesizer::add-patch rack "VCO-1" :out "WAVE-WRITER" :channel-1)
    (cl-synthesizer::add-patch rack "VCO-2" :out "WAVE-WRITER" :channel-2)
    (dotimes (i 100000)
      (cl-synthesizer::update-rack rack))
    (cl-synthesizer::shutdown-rack rack))))

;; (synthesizer-example-440hz)
