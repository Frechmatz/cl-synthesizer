(defsystem :cl-synthesizer-examples
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "An audio synthesizer"
  :long-description
  "All examples of cl-synthesizer. 
   ************************************************************************
   For now, examples must also be added to script/cl-synthesizer-examples.lisp
   ************************************************************************
   The purpose of this system is to load all examples in order to execute 
   them via script/run-examples"
  :depends-on (:cl-synthesizer)
  :components ((:module "src/synthesizer/rack"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/modules/vca"
			:serial t
			:components ((:file "example-1")
				     (:file "example-2")))
	       (:module "src/modules/vco"
			:serial t
			:components ((:file "example-1")
				     (:file "example-2")
				     (:file "example-4")))
	       (:module "src/modules/midi-cc-interface"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/modules/midi-sequencer"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/modules/multiple"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/modules/fixed-output"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/modules/mixer"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/modules/ramp"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/modules/sustain"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/monitor/monitor"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/modules/trigger"
			:serial t
			:components ((:file "example-1")))
	       (:module "src/modules/adsr"
			:serial t
			:components ((:file "example-1")
				     (:file "example-2")
				     (:file "example-3")))
	       (:module "script"
			:serial t
			;; Holds registry of examples and the execution function
			:components ((:file "cl-synthesizer-examples")))))
