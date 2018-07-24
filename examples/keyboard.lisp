;;
;;
;; A MIDI Keyboard Instrument
;;
;;

(in-package :cl-synthesizer-examples)

(defun synthesizer-example-keyboard ()
  "Keyboard"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    ;; two voices assigned to left/right of line out due to absence of a mixer module
    (cl-synthesizer:add-module
     rack
     "MIDI-IFC"
     #'cl-synthesizer-modules-midi-interface:midi-interface
     :voice-count 2)
    (cl-synthesizer:add-patch rack "MIDI-IN" :midi-events "MIDI-IFC" :midi-events)
    (flet ((add-voice (prefix voice-number)
	     (declare (optimize (debug 3) (speed 0) (space 0)))
	     (flet ((make-module-name (name)
		      (format nil "~a-~a-~a" prefix voice-number name)))
	       (cl-synthesizer:add-module
		rack (make-module-name "VCO")
		#'cl-synthesizer-modules-vco:vco
		:base-frequency (cl-synthesizer-midi:get-note-number-frequency 0)
		:cv-max 5
		:f-max 13000
		:v-peak 5)
	       (cl-synthesizer:add-module rack (make-module-name "ADSR") #'cl-synthesizer-modules-envelope:envelope
					  :segments '((:duration-ms 1000 :target-cv 5 :required-gate-state :on)
						      (:duration-ms 1000 :target-cv 3 :required-gate-state :on)
						      (:required-gate-state :on)
						      (:duration-ms 1000 :target-cv 0 :required-gate-state :ignore)))
	       (cl-synthesizer:add-module rack (make-module-name "VCA") #'cl-synthesizer-modules-vca:vca)
	       (cl-synthesizer:add-patch rack (make-module-name "VCO") :sine (make-module-name "VCA") :input)
	       (cl-synthesizer:add-patch rack (make-module-name "ADSR") :cv (make-module-name "VCA") :cv)
	       ;; Connect with midi-interface
	       (cl-synthesizer:add-patch
		rack
		"MIDI-IFC"
		(cl-synthesizer-macro-util:make-keyword "CV" voice-number)
		(make-module-name "VCO") :cv)
	       (cl-synthesizer:add-patch
		rack
		"MIDI-IFC"
		(cl-synthesizer-macro-util:make-keyword "GATE" voice-number)
		(make-module-name "ADSR") :gate))))

      ;;
      ;; TODO: Numbering is confusing. When will number be incremented by one and when not?
      ;;
      (add-voice "VOICE" 0)
      (cl-synthesizer:add-patch rack "VOICE-0-VCA" :output "LINE-OUT" :channel-1)
      (add-voice "VOICE" 1)
      (cl-synthesizer:add-patch rack "VOICE-1-VCA" :output "LINE-OUT" :channel-2)
      
      ;; Add LINE-OUT Monitor
      (cl-synthesizer-monitor:add-monitor
       rack
       #'cl-synthesizer-monitor-wave-handler:wave-file-handler
       '((:channel-1 "LINE-OUT" :input-socket :channel-1) (:channel-2 "LINE-OUT" :input-socket :channel-2))
       :filename "/Users/olli/waves/lineout.wav")

      ;; Add ADSR Monitor
      (cl-synthesizer-monitor:add-monitor
       rack
       #'cl-synthesizer-monitor-wave-handler:wave-file-handler
       '((:channel-1 "VOICE-0-ADSR" :input-socket :gate)
	 (:channel-2 "VOICE-0-ADSR" :output-socket :cv)
	 (:channel-3 "LINE-OUT" :input-socket :channel-1)
	 (:channel-4 "VOICE-1-ADSR" :input-socket :gate)
	 (:channel-5 "VOICE-1-ADSR" :output-socket :cv)
	 (:channel-6 "LINE-OUT" :input-socket :channel-2)
	 )
       :filename "/Users/olli/waves/adsr.wav")
      rack)))

;;(cl-synthesizer-util:play-rack (cl-synthesizer-examples::synthesizer-example-keyboard) 10 :attach-speaker t :attach-midi t)




