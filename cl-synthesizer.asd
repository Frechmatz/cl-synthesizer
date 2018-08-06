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
		(:module "src/synthesizer"
			:serial t
			:components ((:file "packages")
				     (:file "assembly-error")
				     (:file "invalid-arguments-error")
				     (:file "environment")
				     (:file "rack")
				     (:file "line-out-adapter")
				     (:file "midi-in-adapter")
				     ))
	       (:module "src/core"
			:serial t
			:components ((:file "packages")
				     (:file "linear-converter")
				     (:file "phase-waveform-converter")
				     (:file "phase-generator")
				     (:file "trigger")
				     (:file "function-array")
				     ))
		(:module "src/midi"
			:serial t
			:components ((:file "packages")
				     (:file "tuning")
				     (:file "event")
				     (:file "cc-handler")
				     (:file "voice-manager")))
	       (:module "src/modules"
			:serial t
			:components ((:file "packages")
				     (:file "vco")
				     (:file "multiple")
				     (:file "envelope")
				     (:file "vca")
				     (:file "midi-interface")
				     (:file "fixed-output")
				     ))
	       (:module "src/device"
			:serial t
			:components ((:file "packages")
				     (:file "speaker")
				     (:file "wave-file-writer")
				     (:file "midi-device")
				     (:file "midi-sequencer")))
	       (:module "src/monitor"
			:serial t
			:components ((:file "packages")
				     (:file "monitor")
				     (:file "wave-file-handler")))
	       (:module "src/vendor"
			:serial t
			:components ((:file "packages")
				     (:file "arturiaminilabmk2")))
	       (:module "util"
			:serial t
			:components ((:file "packages")
				     (:file "play-rack")))
	       (:module "examples"
			:serial t
			:components ((:file "packages")
				     (:file "vcoexample")
				     (:file "midiccexample")
				     (:file "keyboard")
				     (:file "modulated-envelope")))
	       ;; Examples provided by Modules as documentation
	       ;; not needed by cl-synthesizer but include them to ensure that they are at least loadable 
	       (:module "src/modules/vca"
			:serial t
			:components ((:file "example-1")
				     (:file "example-2")))
			))

