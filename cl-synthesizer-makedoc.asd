(defsystem :cl-synthesizer-makedoc
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description "An audio synthesizer"
  :depends-on (:cl-synthesizer :cl-html-readme :docparser)
  :components ((:module "patches"
			:serial t
			:components ((:file "siren")
				     (:file "sine")))
	       (:module "makedoc"
			:serial t
			:components ((:file "packages")
				     (:file "make-readme")))))

