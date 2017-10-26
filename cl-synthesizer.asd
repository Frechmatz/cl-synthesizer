(defsystem :cl-synthesizer
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
	       :alexandria
	       :cl-out123
	       :cl-wave
	       :verbose)
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "synthesizer")))
	       (:module "src/modules"
			:serial t
			:components ((:file "packages")
				     (:file "constants")
				     (:file "macro-util")
				     (:file "sinus-vco")
				     (:file "vco")
				     (:file "wave-file-writer")
				     (:file "speaker")
				     ))
	       (:module "examples"
			:serial t
			:components ((:file "packages")
				     (:file "play")
				     (:file "440Hz")
				     (:file "vcoexample")
				     )
			)))



