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
				     (:file "function-array")
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
				     (:file "envelope")
				     (:file "adder")
				     (:file "cv-to-trigger")
				     (:file "mixer")
				     (:file "ramp-stage")))
	       (:module "test"
			:serial t
			:components ((:file "packages")
				     (:file "test-util")))
	       (:module "test/core"
			:serial t
			:components ((:file "test-waveform")
				     (:file "test-function-array")))
	       (:module "test/midi"
			:serial t
			:components ((:file "test-voice-manager-voice")
				     (:file "test-voice-manager")))
	       (:module "test/modules/midi-interface"
			:serial t
			:components ((:file "test-interface")))
	       (:module "test/modules/midi-cc-interface"
			:serial t
			:components ((:file "test-midi-cc-interface")))
	       (:module "test/modules/envelope"
			:serial t
			:components ((:file "test-envelope")
				     (:file "test-envelope-validation")))
	       (:module "test/modules/vca"
			:serial t
			:components ((:file "test-vca")))
	       (:module "test/modules/vco"
			:serial t
			:components ((:file "test-vco")))
	       (:module "test/modules/adder"
			:serial t
			:components ((:file "test-adder")))
	       (:module "test/modules/mixer"
			:serial t
			:components ((:file "test-mixer")))
	       (:module "test/modules/cv-to-trigger"
			:serial t
			:components ((:file "test-cv-to-trigger")))
	       (:module "test/modules/stage"
			:serial t
			:components ((:file "test-stage")))
	       (:module "test/synthesizer"
			:serial t
			:components ((:file "test-assembly")
				     (:file "test-patching")
				     (:file "test-update")
				     (:file "test-nested-racks")
				     (:file "test-find-module")))))

