(defsystem :cl-synthesizer-test
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :depends-on (
	       ;;:cl-portaudio
	       ;;:flexi-streams
	       :alexandria
	       :lisp-unit
	       :cl-wave
	       :verbose
	       :bordeaux-threads
	       )
  :components ((:module "src/eventlogger"
			:serial t
			:components ((:file "packages")
				     (:file "event-logger")
				     (:file "event-aggregator")
				     (:file "console")
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
				     (:file "midi-in")))
	       (:module "src/core"
			:serial t
			:components ((:file "packages")
				     (:file "linear-converter")
				     (:file "exponential-converter")
				     (:file "phase-waveform-converter")
				     (:file "phase-generator")
				     (:file "trigger")))
	       (:module "test"
			:serial t
			:components ((:file "packages")
				     (:file "test-util")))
	       (:module "test/core"
			:serial t
			:components (
				     (:file "test-trigger")
				     (:file "test-exponential-converter")
				     (:file "test-waveform")
				     ))
	       (:module "test/synthesizer"
			:serial t
			:components ((:file "test-assembly")
				     (:file "test-patching")
				     (:file "test-update")))))

