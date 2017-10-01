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
	       :cl-wave
	       :verbose)
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "synthesizer")))
	       (:module "src/modules"
			:serial t
			:components ((:file "packages")
				     (:file "cosinus-vco")
				     (:file "wave-file-writer")
				     ))
	       (:module "src/examples"
			:serial t
			:components ((:file "packages")
				     (:file "440Hz"))
			)))



