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

(defun example-lfo (environment)
  (cl-synthesizer-modules-vco::vco environment :f-0 0.2 :cv-min -2.5 :cv-max 2.5 :f-min 0 :f-max 50 :v-peak 5))

(defun synthesizer-example-vco-lfo-stereo-speaker ()
  "Modulate sine with sine-lfo"
  (let ((rack (make-instance 'cl-synthesizer:rack :environment (cl-synthesizer::make-environment))))
      (cl-synthesizer::add-module rack "LFO-1" #'example-lfo)
      (cl-synthesizer::add-module rack "LFO-2" #'example-lfo)
      (cl-synthesizer::add-module rack "VCO-1" #'example-vco)
      (cl-synthesizer::add-module rack "VCO-2" #'example-vco)
      (cl-synthesizer::add-module rack "WAVE-WRITER-AUDIO"
				  #'cl-synthesizer-modules-wave-file-writer::two-channel-wave-file-writer
				  :filename "/Users/olli/waves/AudioOut.wav")
      (cl-synthesizer::add-module rack "WAVE-WRITER-OSCILLOSCOPE"
				  #'cl-synthesizer-modules-wave-file-writer::four-channel-wave-file-writer
				  :filename "/Users/olli/waves/VCOFourChannelExample.wav")
      (cl-synthesizer::add-module rack "SPEAKER" #'cl-synthesizer-modules-speaker::stereo-speaker
				  :driver "coreaudio")

      (cl-synthesizer::add-patch rack "VCO-1" :sine "WAVE-WRITER-OSCILLOSCOPE" :channel-1)
      (cl-synthesizer::add-patch rack "VCO-2" :sine "WAVE-WRITER-OSCILLOSCOPE" :channel-2)
      (cl-synthesizer::add-patch rack "LFO-1" :sine "WAVE-WRITER-OSCILLOSCOPE" :channel-3)
      (cl-synthesizer::add-patch rack "WAVE-WRITER-OSCILLOSCOPE" :out-3 "VCO-1" :cv)
      (cl-synthesizer::add-patch rack "LFO-2" :sine "WAVE-WRITER-OSCILLOSCOPE" :channel-4)
      (cl-synthesizer::add-patch rack "WAVE-WRITER-OSCILLOSCOPE" :out-4 "VCO-2" :cv)

      (cl-synthesizer::add-patch rack "WAVE-WRITER-OSCILLOSCOPE" :out-1 "WAVE-WRITER-AUDIO" :channel-1)
      (cl-synthesizer::add-patch rack "WAVE-WRITER-OSCILLOSCOPE" :out-2 "WAVE-WRITER-AUDIO" :channel-2)
      (cl-synthesizer::add-patch rack "WAVE-WRITER-AUDIO" :out-1 "SPEAKER" :channel-1)
      (cl-synthesizer::add-patch rack "WAVE-WRITER-AUDIO" :out-2 "SPEAKER" :channel-2)
      
    (let ((duration-secs 5))
      (format t "~%Ticks: ~a~%" (* duration-secs (getf (slot-value rack 'cl-synthesizer::environment) :sample-rate)))
      (dotimes (i (* duration-secs (getf (slot-value rack 'cl-synthesizer::environment) :sample-rate)))
	(cl-synthesizer::update-rack rack))
      (cl-synthesizer::shutdown-rack rack))))

;; TODO: Clicks during play via speaker module :(
;; - check if frequency is greater than sampling rate (there is currently no frequency clipping in place)
;; - check if frequency is negative
;; - generated wave files play fine
;;(synthesizer-example-vco-lfo-stereo-speaker)
