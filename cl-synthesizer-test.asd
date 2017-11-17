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
	       :verbose)
  :components (
	       (:module "src/synthesizer"
			:serial t
			:components ((:file "packages")
				     (:file "assembly-error")
				     (:file "environment")
				     (:file "rack-module")
				     (:file "rack-module-patch")
				     (:file "rack")))
	       (:module "src/core"
			:serial t
			:components ((:file "packages")
				     (:file "linear-converter")
				     (:file "phase-generator")
				     (:file "sine-core")
				     (:file "triangle-core")
				     (:file "saw-core")
				     (:file "square-core")
				     (:file "trigger")))
	       (:module "test"
			:serial t
			:components ((:file "packages")
				     (:file "test-util")))
	       (:module "test/core"
			:serial t
			:components (
				     (:file "test-trigger")
				     (:file "test-sine-core")
				     (:file "test-square-core")
				     (:file "test-triangle-core")
				     (:file "test-saw-core")
				     ))
	       (:module "test/synthesizer"
			:serial t
			:components ((:file "test-assembly")
				     (:file "test-patching")
				     (:file "test-update")))))

