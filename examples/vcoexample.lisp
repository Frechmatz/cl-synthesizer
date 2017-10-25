(in-package :cl-synthesizer-examples)


(defun example-vco (environment)
  (cl-synthesizer-modules-vco::vco environment :f-0 440 :cv-min -5 :cv-max 5 :f-min 15 :f-max 8000 :v-peak 5))

(defun synthesizer-example-vco-stereo-speaker ()
  "Play two sinus waves in stereo"
  (let ((rack (make-instance 'cl-synthesizer:rack :environment (cl-synthesizer::make-environment))))
      (cl-synthesizer::add-module rack "VCO-1" #'example-vco)
      (cl-synthesizer::add-module rack "VCO-2" #'example-vco)
      (cl-synthesizer::add-module rack "WAVE-WRITER"
				  #'cl-synthesizer-modules-wave-file-writer::two-channel-wave-file-writer
				  :filename "/Users/olli/waves/VCOTwoChannelExample.wav")
      (cl-synthesizer::add-module rack "SPEAKER" #'cl-synthesizer-modules-speaker::stereo-speaker
				  :driver "coreaudio")
      (cl-synthesizer::add-patch rack "VCO-1" :sine "WAVE-WRITER" :channel-1)
      (cl-synthesizer::add-patch rack "VCO-2" :sine "WAVE-WRITER" :channel-2)
      (cl-synthesizer::add-patch rack "WAVE-WRITER" :out-1 "SPEAKER" :channel-1)
    (cl-synthesizer::add-patch rack "WAVE-WRITER" :out-2 "SPEAKER" :channel-2)
    (let ((duration-secs 5))
      (format t "~%Ticks: ~a~%" (* duration-secs (getf (slot-value rack 'cl-synthesizer::environment) :sample-rate)))
      (dotimes (i (* duration-secs (getf (slot-value rack 'cl-synthesizer::environment) :sample-rate)))
	(cl-synthesizer::update-rack rack))
      (cl-synthesizer::shutdown-rack rack))))
  
  ;;(synthesizer-example-vco-stereo-speaker)
