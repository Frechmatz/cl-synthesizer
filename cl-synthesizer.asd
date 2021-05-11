(defsystem :cl-synthesizer
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :depends-on (:cl-wave-file-writer)
  :components ((:module "src/util"
		:serial t
		:components ((:file "packages")
			     (:file "macro-util")
			     (:file "lru-set")))
	       (:module "src"
		:serial t
		:components ((:file "packages")
			     (:file "conditions")))
	       (:module "src/synthesizer"
		:serial t
		:components ((:file "environment")
			     (:file "rack-compiler") 
			     (:file "rack")))
	       (:module "src/core"
		:serial t
		:components ((:file "packages")
			     (:file "linear-converter")
			     (:file "phase-waveform-converter")
			     (:file "phase-generator")
			     (:file "normalized-exp")
			     (:file "round-time")))
	       (:module "src/midi"
		:serial t
		:components ((:file "packages")
			     (:file "tuning")
			     (:file "event")
			     (:file "cc-mapper")))
	       (:module "src/modules"
		:serial t
		:components ((:file "packages")))
	       (:module "src/modules/vco"
		:serial t
		:components ((:file "vco")))
	       (:module "src/modules/multiple"
		:serial t
		:components ((:file "multiple")))
	       (:module "src/modules/vca"
		:serial t
		:components ((:file "vca")))
	       (:module "src/modules/midi-polyphonic-interface"
		:serial t
		:components ((:file "midi-polyphonic-interface")))
	       (:module "src/modules/midi-monophonic-interface"
		:serial t
		:components ((:file "midi-monophonic-interface")))
	       (:module "src/modules/midi-relative-cc-interface"
		:serial t
		:components ((:file "midi-relative-cc-interface")))
	       (:module "src/modules/midi-sequencer"
		:serial t
		:components ((:file "midi-sequencer")))
	       (:module "src/modules/fixed-output"
		:serial t
		:components ((:file "fixed-output")))
	       (:module "src/modules/adder"
		:serial t
		:components ((:file "adder")))
	       (:module "src/modules/mixer"
		:serial t
		:components ((:file "mixer")))
	       (:module "src/modules/trigger"
		:serial t
		:components ((:file "trigger")))
	       (:module "src/modules/wave-file-writer"
		:serial t
		:components ((:file "wave-file-writer")))
	       (:module "src/modules/csv-file-writer"
		:serial t
		:components ((:file "csv-file-writer")))
	       (:module "src/modules/ramp"
		:serial t
		:components ((:file "ramp")))
	       (:module "src/modules/sustain"
		:serial t
		:components ((:file "sustain")))
	       (:module "src/modules/adsr"
		:serial t
		:components ((:file "adsr")))
	       (:module "src/monitor"
		:serial t
		:components ((:file "packages")
			     (:file "monitor")
			     (:file "buffer-agent")
			     (:file "wave-file-agent")
			     (:file "csv-file-agent"))))
  :in-order-to ((test-op (test-op "cl-synthesizer/test"))))


(defsystem :cl-synthesizer/test
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Test suite of cl-synthesizer"
  :depends-on (:lisp-unit :cl-synthesizer)
  :components ((:module "test"
		:serial t
		:components ((:file "packages")))
	       (:module "test/test-util"
		:serial t
		:components ((:file "module-helper")
			     (:file "pass-through-module")
			     (:file "update-counter-module")
			     (:file "multiplier-module")
			     (:file "input-adder-module")
			     (:file "expect-error")
			     (:file "zero-crossing-trigger")
			     (:file "output-change-counter")
			     (:file "frequency-counter")
			     (:file "is-approximately")
			     (:file "get-patch")))
	       (:module "test/util"
		:serial t
		:components ((:file "test-lru-set")))
	       (:module "test/core"
		:serial t
		:components ((:file "test-waveform")))
	       (:module "test/midi"
		:serial t
		:components ((:file "test-cc-mapper")))
	       (:module "test/modules"
		:serial t
		:components ((:file "test-adder")
			     (:file "test-trigger")
			     (:file "test-midi-relative-cc-interface")
			     (:file "test-midi-polyphonic-interface")
			     (:file "test-midi-monophonic-interface")
			     (:file "test-midi-sequencer")
			     (:file "test-mixer")
			     (:file "test-ramp")
			     (:file "test-sustain")
			     (:file "test-vca")
			     (:file "test-vco")
			     (:file "test-wave-file-writer")
			     (:file "test-csv-file-writer")))
	       (:module "test/monitor"
		:serial t
		:components ((:file "test-monitor")))
	       (:module "test/synthesizer"
		:serial t
		:components ((:file "test-assembly")
			     (:file "test-patching")
			     (:file "test-update")
			     (:file "test-nested-racks")
			     (:file "test-find-module"))))
  :perform (test-op (o c) (symbol-call :lisp-unit '#:run-tests :all :cl-synthesizer-test)))


(defsystem :cl-synthesizer/doc
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Documentation generation for cl-synthesizer"
  :depends-on (:cl-synthesizer :cl-html-readme :docparser)
  :components ((:module "makedoc/patches"
		:serial t
		:components ((:file "siren")
			     (:file "sine")))
	       (:module "makedoc"
		:serial t
		:components ((:file "packages")
			     (:file "make-doc")))))

(defsystem :cl-synthesizer/examples
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Examples of cl-synthesizer"
  :depends-on (:cl-synthesizer)
  :components ((:module "src/synthesizer"
		:serial t
		:components ((:file "example-sine")
			     (:file "example-voice")))
	       (:module "src/modules/vca"
		:serial t
		:components ((:file "example-1")
			     (:file "example-2")))
	       (:module "src/modules/vco"
		:serial t
		:components ((:file "example-1")
			     (:file "example-2")
			     (:file "example-4")))
	       (:module "src/modules/midi-relative-cc-interface"
		:serial t
		:components ((:file "example-1")))
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
	       (:module "src/modules/ramp"
		:serial t
		:components ((:file "example-1")))
	       (:module "src/modules/sustain"
		:serial t
		:components ((:file "example-1")))
	       (:module "src/modules/trigger"
		:serial t
		:components ((:file "example-1")))
	       (:module "src/modules/adsr"
		:serial t
		:components ((:file "example-1")
			     (:file "example-2")
			     (:file "example-3")))
	       (:module "src/monitor"
		:serial t
		:components ((:file "example-1")
			     (:file "example-2")
			     (:file "example-3")))
	       (:module "src/run-examples"
		:serial t
		:components ((:file "packages")
			     (:file "run-examples")))))


(defsystem :cl-synthesizer/profiling
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Profiling suite of cl-synthesizer."
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

