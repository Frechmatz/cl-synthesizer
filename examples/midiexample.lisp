;;
;;
;; Midi example
;;
;;

(in-package :cl-synthesizer-examples)

(defun example-vco (environment &key (f-0 440) (f-min 15) (f-max 8000))
  (cl-synthesizer-modules-vco::vco environment :f-0 f-0 :cv-min -5 :cv-max 5 :f-min f-min :f-max f-max :v-peak 5))

(defun synthesizer-example-midi ()
  "Midi example"
  (let ((rack (make-instance 'cl-synthesizer:rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "MIDI-IFC" #'cl-synthesizer-modules-midi-interface:midi-interface)
    (cl-synthesizer::add-module rack "VCO-1" #'example-vco)
    (cl-synthesizer::add-module rack "SPEAKER" #'cl-synthesizer-modules-speaker::mono-speaker
				:driver "coreaudio")
    
    (cl-synthesizer::add-patch rack "MIDI-IFC" :out-1 "VCO-1" :cv)
    (cl-synthesizer::add-patch rack "VCO-1" :sine "SPEAKER" :channel-1)
    rack))
      
;;(cl-synthesizer-examples::play-rack (cl-synthesizer-examples::synthesizer-example-midi) 30)
