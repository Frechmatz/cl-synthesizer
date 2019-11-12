(defsystem :cl-synthesizer-test
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
				     (:file "test-find-module")))))

