(defsystem :cl-synthesizer-experimental
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :depends-on (:cl-synthesizer)
  :components ((:module "experimental"
			:serial t
			:components ((:file "packages")
				     (:file "play-rack")))
	       (:module "experimental/vendor"
		:serial t
		:components ((:file "packages")
			     (:file "arturiaminilabmk2")))))

