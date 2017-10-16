
(in-package :cl-synthesizer-examples)


(defun synthesizer-example-440hz-44100-two-channel ()
  "Write two 440Hz channels"
  (let ((rack (make-instance 'cl-synthesizer:rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "VCO-1" #'cl-synthesizer-modules-sinus-vco::sinus-vco :f_0 440)
    (cl-synthesizer::add-module rack "VCO-2" #'cl-synthesizer-modules-sinus-vco::sinus-vco :f_0 440)
    (cl-synthesizer::add-module rack "WAVE-WRITER"
				#'cl-synthesizer-modules-wave-file-writer::two-channel-wave-file-writer
				:filename "/Users/olli/waves/440Hz44100TwoChannelExample.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :out "WAVE-WRITER" :channel-1)
    (cl-synthesizer::add-patch rack "VCO-2" :out "WAVE-WRITER" :channel-2)
    (let ((duration-secs 2))
      (dotimes (i (* duration-secs (getf (slot-value rack 'cl-synthesizer::environment) :sample-rate)))
	(cl-synthesizer::update-rack rack)))
    (cl-synthesizer::shutdown-rack rack)))

;; (synthesizer-example-440hz-44100-two-channel)


(defun synthesizer-example-440hz-44100-one-channel ()
  "Write one 440Hz channel"
  (let ((rack (make-instance 'cl-synthesizer:rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "VCO-1" #'cl-synthesizer-modules-sinus-vco::sinus-vco :f_0 440)
    (cl-synthesizer::add-module rack "WAVE-WRITER" #'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/440Hz44100OneChannelExample.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :out "WAVE-WRITER" :channel-1)
    (let ((duration-secs 2))
      (format t "~%Ticks: ~a~%" (* duration-secs (getf (slot-value rack 'cl-synthesizer::environment) :sample-rate)))
      (dotimes (i (* duration-secs (getf (slot-value rack 'cl-synthesizer::environment) :sample-rate)))
	(cl-synthesizer::update-rack rack))
      (cl-synthesizer::shutdown-rack rack))))

;; (synthesizer-example-440hz-44100-one-channel)

(defun synthesizer-example-440hz-88100-one-channel ()
  "Write one 440Hz channel"
  (let ((rack (make-instance 'cl-synthesizer:rack :environment (cl-synthesizer::make-environment :sample-rate 88100))))
    (cl-synthesizer::add-module rack "VCO-1" #'cl-synthesizer-modules-sinus-vco::sinus-vco :f_0 440)
    (cl-synthesizer::add-module rack "WAVE-WRITER" #'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/440Hz88100OneChannelExample.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :out "WAVE-WRITER" :channel-1)
    (let ((duration-secs 2))
      (dotimes (i (* duration-secs (getf (slot-value rack 'cl-synthesizer::environment) :sample-rate)))
	(cl-synthesizer::update-rack rack))
      (cl-synthesizer::shutdown-rack rack))))

;; (synthesizer-example-440hz-88100-one-channel)

(defun synthesizer-example-440hz-22000-one-channel ()
  "Write one 440Hz channel"
  (let ((rack (make-instance 'cl-synthesizer:rack :environment (cl-synthesizer::make-environment :sample-rate 22000))))
    (cl-synthesizer::add-module rack "VCO-1" #'cl-synthesizer-modules-sinus-vco::sinus-vco :f_0 440)
    (cl-synthesizer::add-module rack "WAVE-WRITER" #'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/440Hz22000OneChannelExample.wav")
    (cl-synthesizer::add-patch rack "VCO-1" :out "WAVE-WRITER" :channel-1)
    (let ((duration-secs 2))
      (dotimes (i (* duration-secs (getf (slot-value rack 'cl-synthesizer::environment) :sample-rate)))
	(cl-synthesizer::update-rack rack))
      (cl-synthesizer::shutdown-rack rack))))

;; (synthesizer-example-440hz-22000-one-channel)
