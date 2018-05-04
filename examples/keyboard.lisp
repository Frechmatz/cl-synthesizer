;;
;;
;; A Keyboard Instrument
;;
;;

(in-package :cl-synthesizer-examples)

(defun synthesizer-example-keyboard ()
  "Keyboard"
  (let ((rack (cl-synthesizer:create-rack :environment (cl-synthesizer::make-environment))))
    ;; two voices assigned to left/right of line out due to absence of a mixer module
    (cl-synthesizer::add-module
     rack
     "MIDI-IFC"
     #'cl-synthesizer-modules-midi-interface:midi-interface
     :voice-count 2)
    (cl-synthesizer::add-patch rack "MIDI-IN" :midi-events "MIDI-IFC" :midi-events)
    (flet ((add-voice (prefix voice-number)
	     (flet ((make-module-name (name)
		       (format nil "~a-~a-~a" prefix voice-number name)))
	       (cl-synthesizer::add-module
		rack (make-module-name "VCO")
		#'cl-synthesizer-modules-vco:vco
		:footage (cl-synthesizer-midi:get-note-number-frequency 0)
		:cv-max 5
		:f-max 13000
		:v-peak 5)
	       (cl-synthesizer::add-module rack (make-module-name "ADSR") #'cl-synthesizer-modules-adsr:adsr)
	       (cl-synthesizer::add-module rack (make-module-name "VCA") #'cl-synthesizer-modules-vca:vca)
	       (cl-synthesizer::add-patch rack (make-module-name "VCO") :sine (make-module-name "VCA") :input)
	       (cl-synthesizer::add-patch rack (make-module-name "ADSR") :cv (make-module-name "VCA") :cv)
	       ;; Connect with midi-interface
	       (cl-synthesizer::add-patch
		rack
		"MIDI-IFC"
		(cl-synthesizer-macro-util:make-keyword "CV" voice-number)
		(make-module-name "VCO") :cv)
	       (cl-synthesizer::add-patch
		rack
		"MIDI-IFC"
		(cl-synthesizer-macro-util:make-keyword "GATE" voice-number)
		(make-module-name "ADSR") :gate)
	       (values (make-module-name "VCA") :out))))
      (cl-synthesizer::add-module
       rack "WAVE-WRITER"
       #'cl-synthesizer-modules-wave-file-writer::two-channel-wave-file-writer
       :filename "/Users/olli/waves/lineout.wav")
      (multiple-value-bind (output-module-name output-module-socket)
	  (add-voice "VOICE" 0)
	  ;; Connect voice to wave writer and line out
	  (cl-synthesizer::add-module rack "MULTIPLE-1" #'cl-synthesizer-modules-multiple:multiple-4)
	  (cl-synthesizer::add-patch rack output-module-name output-module-socket "MULTIPLE-1" :input)
	  (cl-synthesizer::add-patch rack "MULTIPLE-1" :out-1 "LINE-OUT" :channel-1)
	  (cl-synthesizer::add-patch rack "MULTIPLE-1" :out-2 "WAVE-WRITER" :channel-1))
      (multiple-value-bind (output-module-name output-module-socket)
	  (add-voice "VOICE" 1)
	;; Connect voice to wave writer and line out
	(cl-synthesizer::add-module rack "MULTIPLE-2" #'cl-synthesizer-modules-multiple:multiple-4)
	(cl-synthesizer::add-patch rack output-module-name output-module-socket "MULTIPLE-2" :input)
	(cl-synthesizer::add-patch rack "MULTIPLE-2" :out-1 "LINE-OUT" :channel-2)
	(cl-synthesizer::add-patch rack "MULTIPLE-2" :out-2 "WAVE-WRITER" :channel-2))
	rack)))
    
;;(cl-synthesizer-util:play-rack (cl-synthesizer-examples::synthesizer-example-keyboard) 20 :attach-speaker t :attach-midi t)




