(defsystem :cl-synthesizer
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :components ((:module "src"
		:serial t
		:components ((:file "packages")
			     (:file "conditions")))
	       (:module "src/synthesizer"
		:serial t
		:components ((:file "api")
			     (:file "environment")
			     (:file "rack-compiler") 
			     (:file "rack")))
	       (:module "src/util"
		:serial t
		:components ((:file "packages")
			     (:file "phase-waveform-converter")
			     (:file "phase-generator")
			     (:file "normalized-exp"))))
  :in-order-to ((test-op (test-op "cl-synthesizer/test"))))

(defsystem :cl-synthesizer/modules
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :depends-on (:cl-synthesizer
	       ;; cl-wave-file-writer dependency to be removed after split into dedicated module systems
	       :cl-wave-file-writer)
  :components ((:module "src/modules/vco"
		:serial t
		:components ((:file "packages")
			     (:file "vco")))
	       (:module "src/modules/multiple"
		:serial t
		:components ((:file "packages")
			     (:file "multiple")))
	       (:module "src/modules/vca"
		:serial t
		:components ((:file "packages")
			     (:file "vca")))
	       (:module "src/modules/midi"
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
		:components ((:file "midi-sequencer")))
	       (:module "src/modules/fixed-output"
		:serial t
		:components ((:file "packages")
			     (:file "fixed-output")))
	       (:module "src/modules/adder"
		:serial t
		:components ((:file "packages")
			     (:file "adder")))
	       (:module "src/modules/mixer"
		:serial t
		:components ((:file "packages")
			     (:file "mixer")))
	       (:module "src/modules/trigger"
		:serial t
		:components ((:file "packages")
			     (:file "trigger")))
	       (:module "src/modules/wave-file-writer"
		:serial t
		:components ((:file "packages")
			     (:file "wave-file-writer")))
	       (:module "src/modules/csv-file-writer"
		:serial t
		:components ((:file "packages")
			     (:file "csv-file-writer")))
	       (:module "src/modules/ramp"
		:serial t
		:components ((:file "packages")
			     (:file "ramp")))
	       (:module "src/modules/sustain"
		:serial t
		:components ((:file "packages")
			     (:file "sustain")))
	       (:module "src/modules/adsr"
		:serial t
		:components ((:file "packages")
			     (:file "adsr"))))
  :in-order-to ((test-op (test-op "cl-synthesizer/test"))))


(defsystem :cl-synthesizer/monitor
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :depends-on (:cl-synthesizer)
  :components ((:module "src/monitor"
		:serial t
		:components ((:file "packages")
			     (:file "monitor"))))
  :in-order-to ((test-op (test-op "cl-synthesizer/test"))))


(defsystem :cl-synthesizer/monitor-buffer
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :depends-on (:cl-synthesizer/monitor)
  :components ((:module "src/monitor/buffer"
		:serial t
		:components ((:file "packages")
			     (:file "agent"))))
  :in-order-to ((test-op (test-op "cl-synthesizer/test"))))

(defsystem :cl-synthesizer/monitor-wave-file
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :depends-on (:cl-synthesizer/monitor :cl-wave-file-writer)
  :components ((:module "src/monitor/wave-file"
		:serial t
		:components ((:file "packages")
			     (:file "agent"))))
  :in-order-to ((test-op (test-op "cl-synthesizer/test"))))

(defsystem :cl-synthesizer/monitor-csv-file
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :depends-on (:cl-synthesizer/monitor)
  :components ((:module "src/monitor/csv-file"
		:serial t
		:components ((:file "packages")
			     (:file "agent"))))
  :in-order-to ((test-op (test-op "cl-synthesizer/test"))))

(defsystem :cl-synthesizer/test
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Test suite of cl-synthesizer"
  :depends-on (:lisp-unit
	       :cl-synthesizer
	       :cl-synthesizer/modules
	       :cl-synthesizer/monitor
	       :cl-synthesizer/monitor-buffer
	       :cl-synthesizer/monitor-csv-file
	       :cl-synthesizer/monitor-wave-file)
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
		:components ((:file "test-waveform")))
	       (:module "test/modules"
		:serial t
		:components ((:file "test-adder")
			     (:file "test-trigger")
			     (:file "test-mixer")
			     (:file "test-ramp")
			     (:file "test-sustain")
			     (:file "test-vca")
			     (:file "test-vco")
			     (:file "test-wave-file-writer")
			     (:file "test-csv-file-writer")))
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
			     (:file "test-expose-socket"))))
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
	       :cl-synthesizer/modules
	       :cl-synthesizer/monitor
	       :cl-synthesizer/monitor-buffer
	       :cl-synthesizer/monitor-csv-file
	       :cl-synthesizer/monitor-wave-file	       
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
  :depends-on (:cl-synthesizer
	       :cl-synthesizer/modules
	       :cl-synthesizer/monitor
	       :cl-synthesizer/monitor-buffer
	       :cl-synthesizer/monitor-csv-file
	       :cl-synthesizer/monitor-wave-file)
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
  :depends-on (:cl-synthesizer
	       :cl-synthesizer/modules
	       :cl-synthesizer/monitor
	       :cl-synthesizer/monitor-buffer
	       :cl-synthesizer/monitor-csv-file
	       :cl-synthesizer/monitor-wave-file)
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

