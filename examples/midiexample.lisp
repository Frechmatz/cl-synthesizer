;;
;;
;; Midi example
;;
;;

(in-package :cl-synthesizer-examples)

(defun midi-example-vco (environment &key (f-0 440) (f-min 440) (f-max 900))
  (cl-synthesizer-modules-vco::vco environment :f-0 f-0 :cv-min -5 :cv-max 5 :f-min f-min :f-max f-max :v-peak 5))

(defun synthesizer-example-midi ()
  "Midi example"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "MIDI-IFC" #'cl-synthesizer-modules-midi-interface:midi-interface)
    (cl-synthesizer::add-module rack "VCO-1" #'midi-example-vco)
    (cl-synthesizer::add-patch rack "MIDI-IFC" :out-1 "VCO-1" :cv)
    (cl-synthesizer::add-patch rack "VCO-1" :sine "LINE-OUT" :channel-1)
    (cl-synthesizer::add-patch rack "VCO-1" :triangle "LINE-OUT" :channel-2)
    rack))
      
;;(cl-synthesizer-examples::play-rack-with-audio-output (cl-synthesizer-examples::synthesizer-example-midi) 10)


(defun synthesizer-example-midi-2 ()
  "Midi example"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "MIDI-IFC" #'cl-synthesizer-modules-midi-interface:midi-interface)
    (cl-synthesizer::add-module rack "VCO-1" #'midi-example-vco :f-0 440 :f-min 440 :f-max 900)
    (cl-synthesizer::add-module rack "VCO-2" #'midi-example-vco :f-0 442 :f-min 440 :f-max 900)
    (cl-synthesizer::add-module rack "MULTIPLE-1" #'cl-synthesizer-modules-multiple::multiple-4)
    
    (cl-synthesizer::add-patch rack "MIDI-IFC" :out-1 "MULTIPLE-1" :input)
    (cl-synthesizer::add-patch rack "MULTIPLE-1" :out-1 "VCO-1" :cv)
    (cl-synthesizer::add-patch rack "MULTIPLE-1" :out-2 "VCO-2" :cv)
    (cl-synthesizer::add-patch rack "VCO-1" :triangle "LINE-OUT" :channel-1)
    (cl-synthesizer::add-patch rack "VCO-2" :triangle "LINE-OUT" :channel-2)
    rack))
      
;;(cl-synthesizer-examples::play-rack-with-audio-output (cl-synthesizer-examples::synthesizer-example-midi-2) 120)
