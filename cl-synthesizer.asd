(defsystem :cl-synthesizer
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :depends-on (:cl-synthesizer/core
	       :cl-synthesizer/system
	       :cl-synthesizer/vco
	       :cl-synthesizer/multiple
	       :cl-synthesizer/vca
	       :cl-synthesizer/midi
	       :cl-synthesizer/fixed-output
	       :cl-synthesizer/adder
	       :cl-synthesizer/mixer
	       :cl-synthesizer/trigger
	       :cl-synthesizer/csv-file-writer
	       :cl-synthesizer/wave-file-writer
	       :cl-synthesizer/ramp
	       :cl-synthesizer/sustain
	       :cl-synthesizer/adsr
	       :cl-synthesizer/monitor
	       :cl-synthesizer/monitor-buffer
	       :cl-synthesizer/monitor-csv-file
	       :cl-synthesizer/monitor-wave-file
               :cl-synthesizer/sound-server-client)
  :in-order-to ((test-op (test-op "cl-synthesizer/test"))))

(defsystem :cl-synthesizer/core
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "The core of cl-synthesizer"
  :long-description "The core of cl-synthesizer"
  :components ((:module "src/synthesizer"
		:serial t
		:components ((:file "packages")
			     (:file "property-list-iterator") 
			     (:file "conditions")
			     (:file "api")
			     (:file "environment")
			     (:file "rack")))))

(defsystem :cl-synthesizer/util
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Utility functions"
  :long-description "Utility functions"
  :components ((:module "src/util"
		:serial t
		:components ((:file "packages")
			     (:file "phase-waveform-converter")
			     (:file "phase-generator")
			     (:file "normalized-exp")))))

(defsystem :cl-synthesizer/system
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "System module"
  :long-description "System module"
  :depends-on (:cl-synthesizer/core)
  :components ((:module "src/modules/system"
		:serial t
		:components ((:file "packages")
			     (:file "system")))))

(defsystem :cl-synthesizer/vco
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "VCO module"
  :long-description "VCO module"
  :depends-on (:cl-synthesizer/core
	       :cl-synthesizer/util)
  :components ((:module "src/modules/vco"
		:serial t
		:components ((:file "packages")
			     (:file "vco")))))

(defsystem :cl-synthesizer/multiple
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Multiple module"
  :long-description "Multiple module"
  :depends-on (:cl-synthesizer/core
	       :cl-synthesizer/util)
  :components ((:module "src/modules/multiple"
		:serial t
		:components ((:file "packages")
			     (:file "multiple")))))

(defsystem :cl-synthesizer/vca
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "VCA module"
  :long-description "VCA module"
  :depends-on (:cl-synthesizer/core
	       :cl-synthesizer/util)
  :components ((:module "src/modules/vca"
		:serial t
		:components ((:file "packages")
			     (:file "vca")))))

(defsystem :cl-synthesizer/midi
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Midi modules"
  :long-description "Midi modules"
  :depends-on (:cl-synthesizer/core
	       :cl-synthesizer/util)
  :components ((:module "src/modules/midi"
		:serial t
		:components ((:file "packages")
			     (:file "tuning")
			     (:file "event")
			     (:file "cc-mapper")
                             (:file "lru-set")))
	       (:module "src/modules/midi/modules/midi-polyphonic-interface"
		:serial t
		:components ((:file "midi-polyphonic-interface")))
	       (:module "src/modules/midi/modules/midi-monophonic-interface"
		:serial t
		:components ((:file "midi-monophonic-interface")))
	       (:module "src/modules/midi/modules/midi-relative-cc-interface"
		:serial t
		:components ((:file "midi-relative-cc-interface")))
	       (:module "src/modules/midi/modules/midi-sequencer"
		:serial t
		:components ((:file "midi-sequencer")))))

(defsystem :cl-synthesizer/fixed-output
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "fixed-output module"
  :long-description "fixed-output module"
  :depends-on (:cl-synthesizer/core
	       :cl-synthesizer/util)
  :components ((:module "src/modules/fixed-output"
		:serial t
		:components ((:file "packages")
			     (:file "fixed-output")))))

(defsystem :cl-synthesizer/adder
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Adder module"
  :long-description "Adder module"
  :depends-on (:cl-synthesizer/core
	       :cl-synthesizer/util)
  :components ((:module "src/modules/adder"
		:serial t
		:components ((:file "packages")
			     (:file "adder")))))

(defsystem :cl-synthesizer/mixer
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Mixer module"
  :long-description "Mixer module"
  :depends-on (:cl-synthesizer/core
	       :cl-synthesizer/util)
  :components ((:module "src/modules/mixer"
		:serial t
		:components ((:file "packages")
			     (:file "mixer")))))

(defsystem :cl-synthesizer/trigger
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Trigger module"
  :long-description "Trigger module"
  :depends-on (:cl-synthesizer/core
	       :cl-synthesizer/util)
  :components ((:module "src/modules/trigger"
		:serial t
		:components ((:file "packages")
			     (:file "trigger")))))

(defsystem :cl-synthesizer/csv-file-writer
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "CSV file writer module"
  :long-description "CSV file writer module"
  :depends-on (:cl-synthesizer/core
	       :cl-synthesizer/util)
  :components ((:module "src/modules/csv-file-writer"
		:serial t
		:components ((:file "packages")
			     (:file "csv-file-writer")))))

(defsystem :cl-synthesizer/wave-file-writer
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Wave file writer module"
  :long-description "Wave file writer module"
  :depends-on (:cl-synthesizer/core
	       :cl-synthesizer/util
	       :cl-wave-file-writer)
  :components ((:module "src/modules/wave-file-writer"
		:serial t
		:components ((:file "packages")
			     (:file "wave-file-writer")))))

(defsystem :cl-synthesizer/ramp
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Ramp module"
  :long-description "Ramp module"
  :depends-on (:cl-synthesizer/core
	       :cl-synthesizer/util)
  :components ((:module "src/modules/ramp"
		:serial t
		:components ((:file "packages")
			     (:file "ramp")))))

(defsystem :cl-synthesizer/sustain
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Sustain module"
  :long-description "Sustain module"
  :depends-on (:cl-synthesizer/core
	       :cl-synthesizer/util)
  :components ((:module "src/modules/sustain"
		:serial t
		:components ((:file "packages")
			     (:file "sustain")))))

(defsystem :cl-synthesizer/adsr
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "ADSR module"
  :long-description "ADSR module"
  :depends-on (:cl-synthesizer/core
	       :cl-synthesizer/util
	       :cl-synthesizer/ramp
	       :cl-synthesizer/sustain)
  :components ((:module "src/modules/adsr"
		:serial t
		:components ((:file "packages")
			     (:file "adsr")))))

(defsystem :cl-synthesizer/monitor
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "A high-level hook interface"
  :long-description "A high-level hook interface"
  :depends-on (:cl-synthesizer/core)
  :components ((:module "src/monitor"
		:serial t
		:components ((:file "packages")
			     (:file "monitor")))))


(defsystem :cl-synthesizer/monitor-buffer
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "A memory buffer based monitor backend"
  :long-description "A memory buffer based monitor backend"
  :depends-on (:cl-synthesizer/monitor)
  :components ((:module "src/monitor/buffer"
		:serial t
		:components ((:file "packages")
			     (:file "agent")))))

(defsystem :cl-synthesizer/monitor-csv-file
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "A CSV file based monitor backend"
  :long-description "A CSV file based monitor backend"
  :depends-on (:cl-synthesizer/monitor)
  :components ((:module "src/monitor/csv-file"
		:serial t
		:components ((:file "packages")
			     (:file "agent")))))

(defsystem :cl-synthesizer/monitor-wave-file
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "A Wave file based monitor backend"
  :long-description "A Wave file based monitor backend"
  :depends-on (:cl-synthesizer/monitor)
  :components ((:module "src/monitor/wave-file"
		:serial t
		:components ((:file "packages")
			     (:file "agent")))))

(defsystem :cl-synthesizer/sound-server-client
  :serial t
  :version "1.0.0"
  :depends-on (:cl-java-sound-client
               :cl-synthesizer/core
	       :cl-synthesizer/monitor
               :cl-synthesizer/monitor-buffer)
  :components ((:module "src/sound-server-client"
		:serial t
		:components ((:file "packages")
			     (:file "client")))))

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
			     (:file "update-notification-module")
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
		:components ((:file "test-waveform")))
	       (:module "test/modules"
		:serial t
		:components ((:file "test-system")
			     (:file "test-adder")
			     (:file "test-trigger")
			     (:file "test-mixer")
			     (:file "test-ramp")
			     (:file "test-sustain")
			     (:file "test-vca")
			     (:file "test-vco")
			     (:file "test-csv-file-writer")
			     (:file "test-wave-file-writer")))
	       (:module "test/modules/midi"
		:serial t
		:components ((:file "test-cc-mapper")
                             (:file "test-lru-set")
			     (:file "test-midi-relative-cc-interface")
			     (:file "test-midi-polyphonic-interface")
			     (:file "test-midi-monophonic-interface")
			     (:file "test-midi-sequencer")))
	       (:module "test/monitor"
		:serial t
		:components ((:file "test-monitor")))
	       (:module "test/synthesizer"
		:serial t
		:components ((:file "test-assembly")
			     (:file "test-patching")
			     (:file "test-update")
			     (:file "test-nested-racks")
			     (:file "test-find-module")
			     (:file "test-expose-socket")
			     (:file "test-hooks"))))
  :perform (test-op (o c) (symbol-call :lisp-unit '#:run-tests :all :cl-synthesizer-test)))


(defsystem :cl-synthesizer/doc
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Documentation generation for cl-synthesizer"
  :depends-on (:cl-synthesizer
	       :cl-html-readme
	       :docparser)
  :components ((:module "makedoc/patches"
		:serial t
		:components ((:file "saw")
			     (:file "frequency-modulated-saw")
			     (:file "two-frequency-modulated-saws")))
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
  :components ((:module "src/synthesizer/examples"
		:serial t
		:components ((:file "module-adder-2")
			     (:file "module-composed-adder-4")
			     (:file "rack-3-adders")
			     (:file "module-expose-state")
			     (:file "rack-expose")))
	       (:module "src/modules/system"
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
			     (:file "example-4")))
	       (:module "src/modules/midi/modules/midi-relative-cc-interface"
		:serial t
		:components ((:file "example-1")))
	       (:module "src/modules/midi/modules/midi-sequencer"
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
	       (:module "src/monitor/csv-file"
		:serial t
		:components ((:file "example-1")))
	       (:module "src/monitor/wave-file"
		:serial t
		:components ((:file "example-1")))
	       (:module "src/monitor/buffer"
		:serial t
		:components ((:file "example-1")))
	       (:module "src/sound-server-client"
		:serial t
		:components ((:file "example-1")
                             (:file "example-2")))
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
			     (:file "profile-compiler")
			     (:file "profile")))))

