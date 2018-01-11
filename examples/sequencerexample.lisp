;;
;;
;; Step sequencer example
;;
;;

(in-package :cl-synthesizer-examples)

(defun sequencer-example-vco (environment &key (f-0 440) (f-min 15) (f-max 8000))
  (cl-synthesizer-modules-vco::vco environment :f-0 f-0 :cv-min -5 :cv-max 5 :f-min f-min :f-max f-max :v-peak 5))

(defun synthesizer-example-step-sequencer ()
  "Step Sequencer example"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
      (cl-synthesizer::add-module rack "SEQUENCER" #'cl-synthesizer-modules-step-sequencer:step-sequencer)
      (cl-synthesizer::add-module rack "LFO-1" #'sequencer-example-vco :f-0 0.5)
      (cl-synthesizer::add-module rack "VCO-1" #'example-vco :f-0 440 :f-min 220 :f-max 660)
      (cl-synthesizer::add-module rack "WAVE-WRITER-OSCILLOSCOPE"
				  #'cl-synthesizer-modules-wave-file-writer::one-channel-wave-file-writer
				  :filename "/Users/olli/waves/sequencer.wav")

      (cl-synthesizer::add-patch rack "LFO-1" :square "SEQUENCER" :step)
      (cl-synthesizer::add-patch rack "SEQUENCER" :out "VCO-1" :cv)
      (cl-synthesizer::add-patch rack "VCO-1" :sine "WAVE-WRITER-OSCILLOSCOPE" :channel-1)
      (cl-synthesizer::add-patch rack "WAVE-WRITER-OSCILLOSCOPE" :out-1 "LINE-OUT" :channel-1)
      rack))
      
;;(cl-synthesizer-examples::play-rack-with-audio-output (cl-synthesizer-examples::synthesizer-example-step-sequencer) 10)
