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
    :components ((:module "src"
			  :serial t
			  :components ((:file "packages")
				       ;; (:file "letsgo")
				       ;;(:file "wave")
				       ;;(:file "440Hz")
				       ;;(:file "rampsquarevco")
				       (:file "synthesizer")))
		 (:module "test"
			  :serial t
			  :components ((:file "packages")
				       (:file "test-util")))
		 (:module "test/synthesizer"
			  :serial t
			  :components ((:file "test-assembly")
				       (:file "test-patching")
				       (:file "test-update")))))

