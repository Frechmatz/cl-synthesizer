;;
;;
;; Midi CC examples
;;


(in-package :cl-synthesizer-examples)

(defun synthesizer-example-midi-single-controller ()
  "Midi example: Modulate frequency via Controller"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "MIDI-IFC" #'cl-synthesizer-modules-midi-interface:midi-interface
				:controllers
				(list
				 (list :socket :controller-1
				       :handler (cl-synthesizer-midi:relative-cc-handler
						 cl-synthesizer-vendor:*arturia-minilab-mk2*
						 (list (list :controller-id :ENCODER-1 :delta-percent 0.01
							     :turn-speed (lambda(offs) (declare (ignore offs)) 1)))
						 :cv-initial 2.5
						 :cv-min 0
						 :cv-max 5))))
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco
				:base-frequency 440
				:cv-max 5
				:f-max 900
				:v-peak 5)

    (cl-synthesizer:add-patch rack "MIDI-IN" :midi-events "MIDI-IFC" :midi-events)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :controller-1 "VCO-1" :cv-lin)
    (cl-synthesizer:add-patch rack "VCO-1" :sine "LINE-OUT" :channel-1)
    (cl-synthesizer:add-patch rack "VCO-1" :triangle "LINE-OUT" :channel-2)
    rack))
      
;;(cl-synthesizer-util:play-rack (cl-synthesizer-examples::synthesizer-example-midi-single-controller) 10 :attach-speaker t :attach-midi t)

(defun synthesizer-example-midi-chained-controllers ()
  "Midi example: Modulate frequency via Controller"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "MIDI-IFC" #'cl-synthesizer-modules-midi-interface:midi-interface
				:controllers
				(list
				 (list :socket :controller-1
				       :handler (cl-synthesizer-midi:relative-cc-handler
					cl-synthesizer-vendor:*arturia-minilab-mk2*
					(list (list :controller-id :ENCODER-1 :delta-percent 0.005)
					      (list :controller-id :ENCODER-9 :delta-percent 0.02))
					:cv-initial 2.5
					:cv-min 0
					:cv-max 5))))
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco
				:base-frequency 440
				:cv-max 5
				:f-max 900
				:v-peak 5)

    (cl-synthesizer:add-patch rack "MIDI-IN" :midi-events "MIDI-IFC" :midi-events)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :controller-1 "VCO-1" :cv-lin)
    (cl-synthesizer:add-patch rack "VCO-1" :sine "LINE-OUT" :channel-1)
    (cl-synthesizer:add-patch rack "VCO-1" :triangle "LINE-OUT" :channel-2)
    rack))
      
;;(cl-synthesizer-util:play-rack (cl-synthesizer-examples::synthesizer-example-midi-chained-controllers) 10 :attach-speaker t :attach-midi t)

(defun synthesizer-example-midi-single-controller-2 ()
  "Midi example: Modulate frequency via Controller"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "MIDI-IFC" #'cl-synthesizer-modules-midi-interface:midi-interface
				:controllers
				(list
				 (list :socket :controller-1
				       :handler (cl-synthesizer-midi:relative-cc-handler
					cl-synthesizer-vendor:*arturia-minilab-mk2*
					(list (list :controller-id :ENCODER-1 :delta-percent 0.01))
					:cv-initial 2.5
					:cv-min 0
					:cv-max 5))))
    (cl-synthesizer:add-module rack "VCO-1"
				#'cl-synthesizer-modules-vco:vco :base-frequency 440 :cv-max 5 :f-max 900 :v-peak 5)
    (cl-synthesizer:add-module rack "VCO-2"
				#'cl-synthesizer-modules-vco:vco :base-frequency 442 :cv-max 5 :f-max 900 :v-peak 5)

    (cl-synthesizer:add-module rack "MULTIPLE-1" #'cl-synthesizer-modules-multiple::multiple-4)

    (cl-synthesizer:add-patch rack "MIDI-IN" :midi-events "MIDI-IFC" :midi-events)
    (cl-synthesizer:add-patch rack "MIDI-IFC" :controller-1 "MULTIPLE-1" :input)
    (cl-synthesizer:add-patch rack "MULTIPLE-1" :out-1 "VCO-1" :cv-lin)
    (cl-synthesizer:add-patch rack "MULTIPLE-1" :out-2 "VCO-2" :cv-lin)
    (cl-synthesizer:add-patch rack "VCO-1" :triangle "LINE-OUT" :channel-1)
    (cl-synthesizer:add-patch rack "VCO-2" :triangle "LINE-OUT" :channel-2)
    rack))
      
;;(cl-synthesizer-util:play-rack (cl-synthesizer-examples::synthesizer-example-midi-single-controller-2) 10 :attach-speaker t :attach-midi t)

