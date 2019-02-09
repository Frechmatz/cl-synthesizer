(defsystem :cl-synthesizer-macos-device
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "MacOS specific device implementations for CL-Synthesizer"
  :long-description "MacOS specific device implementations for CL-Synthesizer"
  :depends-on (:alexandria
	       :cl-out123
	       :verbose
	       :bordeaux-threads
	       :coremidi
	       :queues.simple-cqueue)
  :components ((:module "src/util"
			:serial t
			:components ((:file "packages")
				     (:file "macro-util")))
	       (:module "experimental/device"
			:serial t
			:components ((:file "packages")
				     (:file "speaker")
				     (:file "midi-device")))))

