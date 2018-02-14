;;
;;
;; Midi example
;;
;; TODO: Make sweeps less steppy
;;

(in-package :cl-synthesizer-examples)

(defun synthesizer-example-midi ()
  "Midi example: Modulate frequency via Controller"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "MIDI-IFC" #'cl-synthesizer-modules-midi-interface:midi-interface)
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :footage 440 :cv-max 5 :f-max 900 :v-peak 5)

    (cl-synthesizer::add-patch rack "MIDI-IN" :midi-event "MIDI-IFC" :midi-event)
    (cl-synthesizer::add-patch rack "MIDI-IFC" :out-1 "VCO-1" :cv-lin)
    (cl-synthesizer::add-patch rack "VCO-1" :sine "LINE-OUT" :channel-1)
    (cl-synthesizer::add-patch rack "VCO-1" :triangle "LINE-OUT" :channel-2)
    rack))
      
;;(cl-synthesizer-examples::play-rack (cl-synthesizer-examples::synthesizer-example-midi) 10 :attach-speaker t :attach-midi t)


(defun synthesizer-example-midi-2 ()
  "Midi example: Modulate frequency via Controller"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    (cl-synthesizer::add-module rack "MIDI-IFC" #'cl-synthesizer-modules-midi-interface:midi-interface)
    (cl-synthesizer::add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :footage 440 :cv-max 5 :f-max 900 :v-peak 5)
    (cl-synthesizer::add-module rack "VCO-2"
				#'cl-synthesizer-modules-vco:vco :footage 442 :cv-max 5 :f-max 900 :v-peak 5)

    (cl-synthesizer::add-module rack "MULTIPLE-1" #'cl-synthesizer-modules-multiple::multiple-4)

    (cl-synthesizer::add-patch rack "MIDI-IN" :midi-event "MIDI-IFC" :midi-event)
    (cl-synthesizer::add-patch rack "MIDI-IFC" :out-1 "MULTIPLE-1" :input)
    (cl-synthesizer::add-patch rack "MULTIPLE-1" :out-1 "VCO-1" :cv-lin)
    (cl-synthesizer::add-patch rack "MULTIPLE-1" :out-2 "VCO-2" :cv-lin)
    (cl-synthesizer::add-patch rack "VCO-1" :triangle "LINE-OUT" :channel-1)
    (cl-synthesizer::add-patch rack "VCO-2" :triangle "LINE-OUT" :channel-2)
    rack))
      
;;(cl-synthesizer-examples::play-rack (cl-synthesizer-examples::synthesizer-example-midi-2) 10 :attach-speaker t :attach-midi t)

