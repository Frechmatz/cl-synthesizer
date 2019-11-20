(defsystem :cl-synthesizer-patches
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "Patches for cl-synthesizer"
  :depends-on (:cl-synthesizer)
  :components ((:module "patches"
			:serial t
			:components ((:file "siren")))))
