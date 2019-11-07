(defsystem :cl-synthesizer-profiling
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :depends-on (:cl-synthesizer)
  :components ((:module "profiling"
			:serial t
			:components ((:file "packages")
				     (:file "profile-rack")
				     (:file "profile-vco")
				     (:file "profile-phase-generator")
				     (:file "profile-phase-waveform-converter")
				     (:file "profile-monitor")
				     (:file "profile-midi-sequencer")
				     (:file "profile-csv-file-writer")
				     (:file "profile-wave-file-writer")
				     (:file "profile-midi-polyphonic-interface")
				     (:file "profile-midi-monophonic-interface")
				     (:file "profile-adsr")
				     (:file "profile-mixer")
				     (:file "profile-keyboard")
				     (:file "profile")))))

