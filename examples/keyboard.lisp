;;
;;
;; A Keyboard Instrument
;;
;;

(in-package :cl-synthesizer-examples)

(defun synthesizer-example-keyboard ()
  "Keyboard"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "MIDI-IFC" #'cl-synthesizer-modules-midi-interface:midi-interface)
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco
				:footage (cl-synthesizer-midi:get-note-number-frequency 0)
				:cv-max 5
				:f-max 13000
				:v-peak 5)
    (cl-synthesizer::add-module rack "ADSR-1" #'cl-synthesizer-modules-adsr:adsr)
    (cl-synthesizer::add-module rack "VCA-1" #'cl-synthesizer-modules-vca:vca)
    (cl-synthesizer::add-module rack "VCA-OUT-MULTIPLE" #'cl-synthesizer-modules-multiple:multiple-4)
    (cl-synthesizer::add-module rack "WAVE-WRITER"
				#'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				:filename "/Users/olli/waves/lineout.wav")

    (cl-synthesizer::add-patch rack "MIDI-IN" :midi-event "MIDI-IFC" :midi-event)
    (cl-synthesizer::add-patch rack "MIDI-IFC" :gate-1 "ADSR-1" :gate)
    (cl-synthesizer::add-patch rack "MIDI-IFC" :cv-1 "VCO-1" :cv)

    (cl-synthesizer::add-patch rack "ADSR-1" :cv "VCA-1" :cv)
    (cl-synthesizer::add-patch rack "VCO-1" :sine "VCA-1" :input)
    (cl-synthesizer::add-patch rack "VCA-1" :out "VCA-OUT-MULTIPLE" :input)
    (cl-synthesizer::add-patch rack "VCA-OUT-MULTIPLE" :out-1 "LINE-OUT" :channel-1)
    (cl-synthesizer::add-patch rack "VCA-OUT-MULTIPLE" :out-2 "WAVE-WRITER" :channel-1)
    rack))
    
;;(cl-synthesizer-examples::play-rack (cl-synthesizer-examples::synthesizer-example-keyboard) 20 :attach-speaker t :attach-midi t)


