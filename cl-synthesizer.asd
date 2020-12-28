(defsystem :cl-synthesizer
  :serial t
  :version "0.0.1"
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
		(:module "src/synthesizer"
			:serial t
			:components ((:file "packages")
				     (:file "conditions")
				     (:file "environment")
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
				     (:file "event")))
	       (:module "src/modules"
			:serial t
			:components ((:file "packages")
				     (:file "vco")
				     (:file "multiple")
				     (:file "vca")
				     (:file "midi-polyphonic-interface")
				     (:file "midi-monophonic-interface")
				     (:file "midi-cc-interface")
				     (:file "midi-sequencer")
				     (:file "fixed-output")
				     (:file "adder")
				     (:file "mixer")
				     (:file "trigger")
				     (:file "wave-file-writer")
				     (:file "csv-file-writer")
				     (:file "ramp")
				     (:file "sustain")
				     (:file "adsr")
				     ))
	       (:module "src/monitor"
			:serial t
			:components ((:file "packages")
				     (:file "monitor")
				     (:file "wave-file-handler")
				     (:file "csv-file-handler")))
	       (:module "src/vendor"
			:serial t
			:components ((:file "packages")
				     (:file "arturiaminilabmk2"))))
  :in-order-to ((test-op (test-op "cl-synthesizer/test"))))


(defsystem :cl-synthesizer/test
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
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
	       (:module "test/modules"
			:serial t
			:components ((:file "test-adder")
				     (:file "test-trigger")
				     (:file "test-midi-cc-interface")
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
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :depends-on (:cl-synthesizer :cl-html-readme :docparser)
  :components ((:module "makedoc/patches"
			:serial t
			:components ((:file "siren")
				     (:file "sine")))
	       (:module "makedoc"
			:serial t
			:components ((:file "packages")
				     (:file "make-doc")))))

