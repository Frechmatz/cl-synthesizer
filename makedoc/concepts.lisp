(in-package :cl-synthesizer-makedoc)

(defun get-concepts ()
  `((heading (:name "Environment" :toc t)
	     ,(cl-readme:read-verbatim "makedoc/concepts/environment.html"))
    (heading (:name "Module" :toc t)
	     ,(cl-readme:read-verbatim "makedoc/concepts/module.html"))
    (heading (:name "Rack" :toc t)
	     "<p>Racks contain modules and their connections which each other. The connections are so called \"Patches\". A rack is represented by a property list, which is fully compatible with a module. Beside the module functionality a rack provides Module and Patch management, Hooks and Compilation. Racks are instantiated via \"cl-synthesizer:make-rack\"</p>"
	     ,(cl-readme:read-code "makedoc/concepts/snippet-rack-make-rack.lisp")
	     "<p>After the rack has been created, modules and patches can be added to it.</p>"
	     ,(cl-readme:read-code "makedoc/concepts/snippet-rack-add-modules.lisp")
	     "<p>The rack is now ready to use and a \"tick\" can be processed. When a tick is to be processed, the rack checks if it has been modified, and if yes, a compilation occurs. The compilation step processes the modules and patch table of the rack in order to eliminate/minimize any expensive property list lookups during the actual execution.</p>"
	     ,(cl-readme:read-code "makedoc/concepts/snippet-rack-tick.lisp")
	     "<p>Lets get the :sine output of the \"VCO\" module. The following
snippet illustrates the low-level Api of the synthesizer. The recommended way to
retrieve output values is to use a \"Monitor\”. Monitors are explained in the next chapter.</p>"
	     ,(cl-readme:read-code "makedoc/concepts/snippet-rack-get-output.lisp")
	     "<p>Racks may expose input and output sockets. When a rack exposes sockets it can be used like any other module when populating another rack. The inputs and outputs are accessible by the internal patching of the rack via so called \"Bridge modules\". \"INPUT\" provides the inputs, and \"OUTPUT\" the outputs of the rack.</p>"
	     ,(cl-readme:read-code "makedoc/concepts/snippet-rack-sockets.lisp")
	     "<p>A more comprehensive example can be found <a href=\"https://github.com/Frechmatz/cl-synthesizer/blob/master/patches/siren.lisp\">here</a>.</p>"
	     )
    (heading (:name "Monitor" :toc t)
	     ,(cl-readme:read-verbatim "makedoc/concepts/monitor.html")
	     ,(make-example-header)
	     ,(cl-readme:read-code "src/monitor/monitor/example-1.lisp"))))
