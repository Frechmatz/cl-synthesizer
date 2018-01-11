
(in-package :cl-synthesizer-examples)


(defun synthesizer-example-440hz-44100-two-channel ()
  "Write two 440Hz channels"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "VCO-1" #'cl-synthesizer-modules-sinus-vco::sinus-vco :f_0 440)
    (cl-synthesizer::add-module rack "VCO-2" #'cl-synthesizer-modules-sinus-vco::sinus-vco :f_0 440)
    (cl-synthesizer::add-module rack "WAVE-WRITER"
				#'cl-synthesizer-modules-wave-file-writer::two-channel-wave-file-writer
				:filename "/Users/olli/waves/440Hz44100TwoChannelExample.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :out "WAVE-WRITER" :channel-1)
    (cl-synthesizer::add-patch rack "VCO-2" :out "WAVE-WRITER" :channel-2)
    rack))

;; (play-rack (synthesizer-example-440hz-44100-two-channel) 2)



(defun synthesizer-example-440hz-44100-one-channel ()
  "Write one 440Hz channel"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "VCO-1" #'cl-synthesizer-modules-sinus-vco::sinus-vco :f_0 440)
    (cl-synthesizer::add-module rack "WAVE-WRITER" #'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/440Hz44100OneChannelExample.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :out "WAVE-WRITER" :channel-1)
    rack))

;; (play-rack (synthesizer-example-440hz-44100-one-channel) 2)

(defun synthesizer-example-440hz-88100-one-channel ()
  "Write one 440Hz channel"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment :sample-rate 88100))))
    (cl-synthesizer::add-module rack "VCO-1" #'cl-synthesizer-modules-sinus-vco::sinus-vco :f_0 440)
    (cl-synthesizer::add-module rack "WAVE-WRITER" #'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/440Hz88100OneChannelExample.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :out "WAVE-WRITER" :channel-1)
    rack))

;; (play-rack (synthesizer-example-440hz-88100-one-channel) 4)

(defun synthesizer-example-440hz-22000-one-channel ()
  "Write one 440Hz channel"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment :sample-rate 22000))))
    (cl-synthesizer::add-module rack "VCO-1" #'cl-synthesizer-modules-sinus-vco::sinus-vco :f_0 440)
    (cl-synthesizer::add-module rack "WAVE-WRITER" #'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/440Hz22000OneChannelExample.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :out "WAVE-WRITER" :channel-1)
    rack))

;; (play-rack (synthesizer-example-440hz-22000-one-channel) 2)

(defun synthesizer-example-440hz-44100-mono-speaker ()
  "Play 440Hz mono"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "VCO-1" #'cl-synthesizer-modules-sinus-vco::sinus-vco :f_0 440)
    (cl-synthesizer::add-patch rack "VCO-1" :out "LINE-OUT" :channel-1)
    rack))

;; (play-rack-with-audio-output (synthesizer-example-440hz-44100-mono-speaker) 4)

(defun synthesizer-example-440hz-44100-stereo-speaker ()
  "Play two sinus waves in stereo"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "VCO-1" #'cl-synthesizer-modules-sinus-vco::sinus-vco :f_0 440)
    (cl-synthesizer::add-module rack "VCO-2" #'cl-synthesizer-modules-sinus-vco::sinus-vco :f_0 442)
    (cl-synthesizer::add-patch rack "VCO-1" :out "LINE-OUT" :channel-1)
    (cl-synthesizer::add-patch rack "VCO-2" :out "LINE-OUT" :channel-2)
    rack))

;; (play-rack-with-audio-output (synthesizer-example-440hz-44100-stereo-speaker) 10)


