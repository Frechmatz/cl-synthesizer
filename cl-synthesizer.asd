(defsystem :cl-synthesizer
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :depends-on (
	       ;;:cl-portaudio
	       :alexandria
	       :cl-out123
	       :cl-wave
	       :verbose
	       :bordeaux-threads
	       )
  :components ((:module "src/synthesizer"
			:serial t
			:components ((:file "packages")
				     (:file "assembly-error")
				     (:file "environment")
				     (:file "rack-module")
				     (:file "rack-module-patch")
				     (:file "rack")
				     ))
	       (:module "src/core"
			:serial t
			:components ((:file "packages")
				     (:file "linear-converter")
				     (:file "phase-waveform-converter")
				     (:file "phase-generator")
				     (:file "trigger")
				     ))
	       (:module "src/modules"
			:serial t
			:components ((:file "packages")
				     (:file "constants")
				     (:file "macro-util")
				     (:file "sinus-vco")
				     (:file "vco")
				     (:file "wave-file-writer")
				     (:file "speaker")
				     (:file "step-sequencer")
				     ))
	       (:module "examples"
			:serial t
			:components ((:file "packages")
				     (:file "play")
				     (:file "440Hz")
				     (:file "vcoexample")
				     (:file "sequencerexample")
				     )
			)))



