(in-package :cl-synthesizer-makedoc)

;;
;; Todos
;; - define with-heading macros to reduce bloat
;;

(defun make-function-string (f)
  (concatenate
   'string
   "<p>"
   (cl-readme:sbcl-make-function-decl f)
   "</p><p>"
   (documentation f 'function)
   "</p>"))

(defun make-condition-string (c)
  (concatenate
   'string
   "<b>" (string-downcase (symbol-name c)) "</b>"
   "<p>"
   (documentation c 'type)
   "</p>"))

(defun make-package-string (p)
  (concatenate
   'string
   "<p><b>" (documentation (find-package p) t)
   "</b></p>"))

(defun get-doc ()
  (let ((tree
	 `("<html>"
	   "<head>"
	   ;; "<link rel=\"stylesheet\" href=\"//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic\">"
	   ;; "<link rel=\"stylesheet\" href=\"//cdn.rawgit.com/necolas/normalize.css/master/normalize.css\">"
	   ;; "<link rel=\"stylesheet\" href=\"//cdn.rawgit.com/milligram/milligram/master/dist/milligram.min.css\">"
	   "</head>"
	   "<body>"
	   (semantic (:name "header")
		     (heading
		      (:name "cl-synthesizer")
		      ,(cl-readme:read-verbatim "makedoc/introduction.html")
		      ,(cl-readme:read-verbatim "makedoc/principles.html"))
		     "<p>The source code of cl-synthesizer can be found <a href=\"https://github.com/Frechmatz/cl-synthesizer\">here</a>.</p>")
	   (semantic (:name "nav")
 		     (heading (:name "Table of contents") TOC))
	   (semantic (:name "section")
		     (heading (:name "Installation" :toc t)
			      ,(cl-readme:read-verbatim "makedoc/installation.html"))
		     (heading (:name "Examples" :toc t)
			      ,(make-package-string 'cl-synthesizer-rack-example-sine)
			      ,(cl-readme:read-code "src/synthesizer/rack/example-sine.lisp")
			      ,(make-package-string 'cl-synthesizer-rack-example-voice)
			      ,(cl-readme:read-code "src/synthesizer/rack/example-voice.lisp"))
		     (heading (:name "API" :toc t)
			      (heading (:toc t :name "Environment")
				       (heading (:toc t :name "make-environment")
						,(make-function-string 'cl-synthesizer:make-environment)))
			      (heading (:toc t :name "Rack")
				       (heading (:toc t :name "make-rack")
						,(make-function-string 'cl-synthesizer:make-rack))
				       (heading (:toc t :name "add-module")
						,(make-function-string 'cl-synthesizer:add-module))
				       (heading (:toc t :name "add-patch")
						,(make-function-string 'cl-synthesizer:add-patch))
				       (heading (:toc t :name "get-module")
						,(make-function-string 'cl-synthesizer:get-module))
				       (heading (:toc t :name "get-module-name")
						,(make-function-string 'cl-synthesizer:get-module-name))
				       (heading (:toc t :name "find-module")
						,(make-function-string 'cl-synthesizer:find-module))
				       (heading (:toc t :name "get-patches")
						,(make-function-string 'cl-synthesizer:get-patches))
				       (heading (:toc t :name "get-modules")
						,(make-function-string 'cl-synthesizer:get-modules))
				       (heading (:toc t :name "add-hook")
						,(make-function-string 'cl-synthesizer:add-hook))
				       (heading (:toc t :name "play-rack")
						,(make-function-string 'cl-synthesizer:play-rack))
				       (heading (:toc t :name "is-rack")
						,(make-function-string 'cl-synthesizer:is-rack))
				       (heading (:toc t :name "get-environment")
						,(make-function-string 'cl-synthesizer:get-environment)))
			      (heading
			       (:toc t :name "Modules")
			       (heading (:toc t :name "VCO")
					,(make-function-string 'cl-synthesizer-modules-vco:make-module)
					,(cl-readme:read-code "src/modules/vco/example-1.lisp"))
			       (heading (:toc t :name "VCA")
					,(make-function-string 'cl-synthesizer-modules-vca:make-module)
					,(cl-readme:read-code "src/modules/vca/example-1.lisp"))
			       (heading (:toc t :name "ADSR")
					,(make-function-string 'cl-synthesizer-modules-adsr:make-module)
					,(cl-readme:read-code "src/modules/adsr/example-1.lisp"))
			       (heading (:toc t :name "Multiple")
					,(make-function-string 'cl-synthesizer-modules-multiple:make-module)
					,(cl-readme:read-code "src/modules/multiple/example-1.lisp"))
			       (heading (:toc t :name "MIDI Polyphonic Interface")
					,(make-function-string 'cl-synthesizer-modules-midi-polyphonic-interface:make-module))
			       (heading (:toc t :name "MIDI Monophonic Interface")
					,(make-function-string 'cl-synthesizer-modules-midi-monophonic-interface:make-module))
			       (heading (:toc t :name "MIDI CC Interface")
					,(make-function-string 'cl-synthesizer-modules-midi-cc-interface:make-module)
					,(cl-readme:read-code "src/modules/midi-cc-interface/example-1.lisp"))
			       (heading (:toc t :name "MIDI Sequencer")
					,(make-function-string 'cl-synthesizer-modules-midi-sequencer:make-module)
					,(cl-readme:read-code "src/modules/midi-sequencer/example-1.lisp"))
			       (heading (:toc t :name "Fixed Output")
					,(make-function-string 'cl-synthesizer-modules-fixed-output:make-module)
					,(cl-readme:read-code "src/modules/fixed-output/example-1.lisp"))
			       (heading (:toc t :name "Adder")
					,(make-function-string 'cl-synthesizer-modules-adder:make-module))
			       (heading (:toc t :name "Mixer")
					,(make-function-string 'cl-synthesizer-modules-mixer:make-module)
					,(cl-readme:read-code "src/modules/mixer/example-1.lisp"))
			       (heading (:toc t :name "Trigger")
					,(make-function-string 'cl-synthesizer-modules-trigger:make-module)
					,(cl-readme:read-code "src/modules/trigger/example-1.lisp"))
			       (heading (:toc t :name "Ramp")
					,(make-function-string 'cl-synthesizer-modules-ramp:make-module)
					,(cl-readme:read-code "src/modules/ramp/example-1.lisp"))
			       (heading (:toc t :name "Sustain")
					,(make-function-string 'cl-synthesizer-modules-sustain:make-module)
					,(cl-readme:read-code "src/modules/sustain/example-1.lisp"))
			       (heading (:toc t :name "Wave File Writer")
					,(make-function-string 'cl-synthesizer-modules-wave-file-writer:make-module))
			       (heading (:toc t :name "CSV File Writer")
					,(make-function-string 'cl-synthesizer-modules-csv-file-writer:make-module)))
			      (heading (:toc t :name "Monitor")
				       (heading (:toc t :name "add-monitor")
						,(make-function-string 'cl-synthesizer-monitor:add-monitor)
						,(cl-readme:read-code "src/monitor/monitor/example-1.lisp"))
				       (heading (:toc t :name "wave-handler")
						,(make-function-string 'cl-synthesizer-monitor-wave-handler:make-handler))
				       (heading (:toc t :name "csv-handler")
						,(make-function-string 'cl-synthesizer-monitor-csv-handler:make-handler)))
			      (heading (:toc t :name "MIDI")
				       (heading (:toc t :name "MIDI Event"))
				       ,(make-function-string 'cl-synthesizer-midi-event:make-control-change-event)
				       ,(make-function-string 'cl-synthesizer-midi-event:make-note-on-event)
				       ,(make-function-string 'cl-synthesizer-midi-event:make-note-off-event)
				       ,(make-function-string 'cl-synthesizer-midi-event:control-change-eventp)
				       ,(make-function-string 'cl-synthesizer-midi-event:note-on-eventp)
				       ,(make-function-string 'cl-synthesizer-midi-event:note-off-eventp)
				       ,(make-function-string 'cl-synthesizer-midi-event:get-channel)
				       ,(make-function-string 'cl-synthesizer-midi-event:get-controller-number)
				       ,(make-function-string 'cl-synthesizer-midi-event:get-controller-value)
				       ,(make-function-string 'cl-synthesizer-midi-event:get-note-number)
				       ,(make-function-string 'cl-synthesizer-midi-event:get-velocity)
				       (heading (:toc t :name "MIDI Utilities")
						,(make-function-string 'cl-synthesizer-midi:get-note-number-frequency)))
			      (heading (:toc t :name "Conditions"))
			      ,(make-condition-string 'cl-synthesizer:assembly-error))
		     (heading (:name "Acknowledgements" :toc t)
			      ,(cl-readme:read-verbatim "makedoc/acknowledge.html")))
	   (semantic (:name "footer")
		     "<p><small>Generated " ,(cl-readme:current-date) "</small></p>")
	   "</body></html>")))
    tree))


(defclass cl-synthesizer-readme-writer (cl-readme:html-writer) ())

(defmethod cl-readme:open-semantic ((writer cl-synthesizer-readme-writer) semantic-element-settings)
  (format nil "<~a class=\"container\">" (getf semantic-element-settings :name)))

(defun make-readme ()
  (let ((cl-readme:*home-directory* "/Users/olli/src/lisp/cl-synthesizer/")
	(cl-readme:*tab-width* 8))
    (with-open-file (fh (cl-readme:make-path "docs/index.html")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf-8)
      (let ((w (make-instance 'cl-synthesizer-readme-writer)))
	(cl-readme:doc-to-html w fh (get-doc))))
  "DONE"))

;;(make-readme)

