(defsystem :cl-synthesizer
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :depends-on (:alexandria
	       :cl-out123
	       :cl-wave
	       :verbose
	       :bordeaux-threads
	       :coremidi
	       :queues.simple-cqueue)
  :components ((:module "src/util"
			:serial t
			:components ((:file "packages")
				     (:file "macro-util")))
		(:module "src/eventlogger"
			:serial t
			:components ((:file "packages")
				     (:file "event-logger")
				     (:file "event-aggregator")
				     (:file "console")
				     ))
		(:module "src/midi"
			:serial t
			:components ((:file "packages")
				     (:file "tuning")
				     (:file "event")
				     (:file "voice-manager")
				     ))
		(:module "src/synthesizer"
			:serial t
			:components ((:file "packages")
				     (:file "assembly-error")
				     (:file "environment")
				     (:file "rack-module")
				     (:file "rack-module-patch")
				     (:file "rack")
				     (:file "line-out")
				     (:file "midi-in")
				     ))
	       (:module "src/core"
			:serial t
			:components ((:file "packages")
				     (:file "linear-converter")
				     (:file "exponential-converter")
				     (:file "phase-waveform-converter")
				     (:file "phase-generator")
				     (:file "trigger")
				     ))
	       (:module "src/modules"
			:serial t
			:components ((:file "packages")
				     (:file "vco")
				     (:file "multiple")
				     (:file "wave-file-writer")
				     (:file "adsr")
				     (:file "vca")
				     (:file "midi-interface")
				     (:file "fixed-output")
				     ))
	       (:module "src/device"
			:serial t
			:components ((:file "packages")
				     (:file "speaker")
				     (:file "midi-device")))
	       (:module "src/vendor"
			:serial t
			:components ((:file "packages")
				     (:file "vendor")
				     (:file "cc-handler")
				     (:file "arturiaminilabmk2")))
	       (:module "examples"
			:serial t
			:components ((:file "packages")
				     (:file "play")
				     (:file "vcoexample")
				     (:file "midiexample")
				     (:file "keyboard")
				     )
			)))



