(defsystem :cl-synthesizer-test
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :depends-on (:alexandria
	       :lisp-unit)
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
				     (:file "rack")))
	       (:module "src/core"
			:serial t
			:components ((:file "packages")
				     (:file "linear-converter")
				     (:file "phase-waveform-converter")
				     (:file "phase-generator")
				     (:file "round-time")))
		(:module "src/midi"
			:serial t
			:components ((:file "packages")
				     (:file "tuning")
				     (:file "event")
				     (:file "voice-manager")))
	       (:module "src/modules"
			:serial t
			:components ((:file "packages")
				     (:file "midi-interface")
				     (:file "midi-cc-interface")
				     (:file "vca")
				     (:file "vco")
				     (:file "adder")
				     (:file "trigger")
				     (:file "mixer")
				     (:file "ramp")
				     (:file "sustain")))
	       (:module "test"
			:serial t
			:components ((:file "packages")))
	       (:module "test/test-util"
			:serial t
			:components ((:file "mirror-module")
				     (:file "update-counter-module")
				     (:file "multiplier-module")
				     (:file "input-adder-module")
				     (:file "expect-error")
				     (:file "zero-crossing-trigger")
				     (:file "frequency-counter")
				     (:file "is-approximately")))
	       (:module "test/core"
			:serial t
			:components ((:file "test-waveform")))
	       (:module "test/midi"
			:serial t
			:components ((:file "test-voice-manager-voice")
				     (:file "test-voice-manager")))
	       (:module "test/modules"
			:serial t
			:components ((:file "test-adder")
				     (:file "test-trigger")
				     (:file "test-midi-cc-interface")
				     (:file "test-midi-interface")
				     (:file "test-mixer")
				     (:file "test-ramp")
				     (:file "test-sustain")
				     (:file "test-vca")
				     (:file "test-vco")))
	       (:module "test/synthesizer"
			:serial t
			:components ((:file "test-assembly")
				     (:file "test-patching")
				     (:file "test-update")
				     (:file "test-nested-racks")
				     (:file "test-find-module")))))

