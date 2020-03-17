(in-package :cl-synthesizer-makedoc)

(defun get-concepts ()
  `((heading (:name "Environment" :toc t)
	     ,(cl-readme:read-verbatim "makedoc/concepts/environment.html"))
    (heading (:name "Module" :toc t)
	     ,(cl-readme:read-verbatim "makedoc/concepts/module.html"))
    (heading (:name "Rack" :toc t)
	     "<p>Racks contain modules and their connections which each other. The connections are so called \"Patches\"."
	     "Racks are instantiated via \"cl-synthesizer:make-rack\"</p>"
	     ,(cl-readme:read-code "makedoc/concepts/snippet-rack-make-rack.lisp")
	     "<p>After the rack has been created, modules and patches can be added to it.</p>"
	     ,(cl-readme:read-code "makedoc/concepts/snippet-rack-add-modules.lisp")
	     "<p>The rack is now ready to use and a \"tick\" can be processed.</p>"
	     ,(cl-readme:read-code "makedoc/concepts/snippet-rack-tick.lisp")
	     )
    (heading (:name "Monitor" :toc t)
	     ,(cl-readme:read-verbatim "makedoc/concepts/monitor.html")
	     ,(make-example-header)
	     ,(cl-readme:read-code "src/monitor/monitor/example-1.lisp"))))
