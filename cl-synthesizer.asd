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
	       :cl-wave)
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
				     (:file "play-rack")))
	       (:module "src/core"
			:serial t
			:components ((:file "packages")
				     (:file "linear-converter")
				     (:file "phase-waveform-converter")
				     (:file "phase-generator")
				     (:file "function-array")
				     ))
		(:module "src/midi"
			:serial t
			:components ((:file "packages")
				     (:file "tuning")
				     (:file "event")
				     (:file "voice-manager")))
	       (:module "src/modules"
			:serial t
			:components ((:file "packages")
				     (:file "vco")
				     (:file "multiple")
				     (:file "envelope")
				     (:file "vca")
				     (:file "midi-interface")
				     (:file "midi-cc-interface")
				     (:file "midi-sequencer")
				     (:file "fixed-output")
				     (:file "adder")
				     (:file "mixer")
				     (:file "cv-to-trigger")
				     ))
	       (:module "src/monitor"
			:serial t
			:components ((:file "packages")
				     (:file "monitor")
				     (:file "wave-file-handler")
				     (:file "wave-file-writer")
				     (:file "csv-file-handler")
				     (:file "csv-file-writer")))
	       (:module "src/vendor"
			:serial t
			:components ((:file "packages")
				     (:file "arturiaminilabmk2")))
	       ;;
	       ;; Examples provided by module implementations.
	       ;;
	       (:module "src/synthesizer/rack"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/modules/vca"
			:serial t
			:components ((:file "example-1")
				     (:file "example-2")))
	       (:module "src/modules/vco"
			:serial t
			:components ((:file "example-1")
				     (:file "example-2")
				     (:file "example-3")))
	       (:module "src/modules/envelope"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/modules/midi-interface"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/modules/midi-cc-interface"
			:serial t
			:components ((:file "example-1")
				     (:file "example-2")))
	       (:module "src/modules/midi-sequencer"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/modules/multiple"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/modules/fixed-output"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/modules/mixer"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/monitor/monitor"
			:serial t
			:components ((:file "example-1")))
	       ))

