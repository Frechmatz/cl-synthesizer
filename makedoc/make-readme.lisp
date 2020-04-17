(in-package :cl-synthesizer-makedoc)


(defun get-readme ()
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
	      (heading (:name "Table of contents") (toc)))
    (semantic (:name "section")
	      (heading (:name "Installation" :toc t)
		       ,(cl-readme:read-verbatim "makedoc/installation.html"))
	      (heading (:name "Examples" :toc t)
		       (heading (:toc t :name ,(documentation (find-package 'cl-synthesizer-patches-sine) t))
				,(cl-readme:read-code "patches/sine.lisp")
				,(make-audio-element "sine.wav"))
		       (heading (:toc t :name ,(documentation (find-package 'cl-synthesizer-patches-siren) t))
				,(cl-readme:read-code "patches/siren.lisp")
				,(make-audio-element "siren.wav")))
	      (heading (:name "Concepts" :toc t)
		       ,(get-concepts))
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
				 ,(make-example-header)
				 ,(cl-readme:read-code "src/modules/vco/example-1.lisp"))
			(heading (:toc t :name "VCA")
				 ,(make-function-string 'cl-synthesizer-modules-vca:make-module)
				 ,(make-example-header)
				 ,(cl-readme:read-code "src/modules/vca/example-1.lisp"))
			(heading (:toc t :name "ADSR")
				 ,(make-function-string 'cl-synthesizer-modules-adsr:make-module)
				 ,(make-example-header)
				 ,(cl-readme:read-code "src/modules/adsr/example-1.lisp"))
			(heading (:toc t :name "Multiple")
				 ,(make-function-string 'cl-synthesizer-modules-multiple:make-module)
				 ,(make-example-header)
				 ,(cl-readme:read-code "src/modules/multiple/example-1.lisp"))
			(heading (:toc t :name "MIDI Polyphonic Interface")
				 ,(make-function-string 'cl-synthesizer-modules-midi-polyphonic-interface:make-module))
			(heading (:toc t :name "MIDI Monophonic Interface")
				 ,(make-function-string 'cl-synthesizer-modules-midi-monophonic-interface:make-module))
			(heading (:toc t :name "MIDI CC Interface")
				 ,(make-function-string 'cl-synthesizer-modules-midi-cc-interface:make-module)
				 ,(make-example-header)
				 ,(cl-readme:read-code "src/modules/midi-cc-interface/example-1.lisp"))
			(heading (:toc t :name "MIDI Sequencer")
				 ,(make-function-string 'cl-synthesizer-modules-midi-sequencer:make-module)
				 ,(make-example-header)
				 ,(cl-readme:read-code "src/modules/midi-sequencer/example-1.lisp"))
			(heading (:toc t :name "Fixed Output")
				 ,(make-function-string 'cl-synthesizer-modules-fixed-output:make-module)
				 ,(make-example-header)
				 ,(cl-readme:read-code "src/modules/fixed-output/example-1.lisp"))
			(heading (:toc t :name "Adder")
				 ,(make-function-string 'cl-synthesizer-modules-adder:make-module))
			(heading (:toc t :name "Mixer")
				 ,(make-function-string 'cl-synthesizer-modules-mixer:make-module)
				 ,(make-example-header)
				 ,(cl-readme:read-code "src/modules/mixer/example-1.lisp"))
			(heading (:toc t :name "Trigger")
				 ,(make-function-string 'cl-synthesizer-modules-trigger:make-module)
				 ,(make-example-header)
				 ,(cl-readme:read-code "src/modules/trigger/example-1.lisp"))
			(heading (:toc t :name "Ramp")
				 ,(make-function-string 'cl-synthesizer-modules-ramp:make-module)
				 ,(make-example-header)
				 ,(cl-readme:read-code "src/modules/ramp/example-1.lisp"))
			(heading (:toc t :name "Sustain")
				 ,(make-function-string 'cl-synthesizer-modules-sustain:make-module)
				 ,(make-example-header)
				 ,(cl-readme:read-code "src/modules/sustain/example-1.lisp"))
			(heading (:toc t :name "Wave File Writer")
				 ,(make-function-string 'cl-synthesizer-modules-wave-file-writer:make-module))
			(heading (:toc t :name "CSV File Writer")
				 ,(make-function-string 'cl-synthesizer-modules-csv-file-writer:make-module)))
		       (heading (:toc t :name "Monitor")
				(heading (:toc t :name "add-monitor")
					 ,(make-function-string 'cl-synthesizer-monitor:add-monitor))
				(heading (:toc t :name "wave-handler")
					 ,(make-function-string 'cl-synthesizer-monitor-wave-handler:make-backend))
				(heading (:toc t :name "csv-handler")
					 ,(make-function-string 'cl-synthesizer-monitor-csv-handler:make-backend)))
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
    "</body></html>"))

(defun get-patches ()
  `("<html>"
    "<head>"
    ;;"<link rel=\"stylesheet\" href=\"//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic\">"
    ;;"<link rel=\"stylesheet\" href=\"//cdn.rawgit.com/necolas/normalize.css/master/normalize.css\">"
    ;;"<link rel=\"stylesheet\" href=\"//cdn.rawgit.com/milligram/milligram/master/dist/milligram.min.css\">"
    "</head>"
    "<script type=\"text/javascript\" src=\"toggledisplay.js\"></script>"
    "<body>"
    (semantic (:name "header")
	      (heading (:name "cl-synthesizer-patches")
		       "Example patches for cl-synthesizer. Work in progress..."
		       "<p>Back to the <a href=\"https://frechmatz.github.io/cl-synthesizer/\">project site.</a></p>"))
    ;;(semantic (:name "nav")
    ;;   (heading (:name "Patches") TOC))
    (semantic (:name "section")
	      ,(make-patch :package 'cl-synthesizer-patches-sine
			   :code "patches/sine.lisp"
			   :wave-file "sine.wav")
	      ,(make-patch :package 'cl-synthesizer-patches-siren
			   :code "patches/siren.lisp"
			   :wave-file "siren.wav"))
    (semantic (:name "footer")
	      "<p><small>Generated " ,(cl-readme:current-date) "</small></p>")
    "</body></html>"))


(defun make-readme ()
  ;; Generate patches
  (cl-synthesizer-patches-siren::run-example)
  (cl-synthesizer-patches-sine::run-example)
  ;; Generate html files
  (let ((cl-readme:*home-directory* "/Users/olli/src/lisp/cl-synthesizer/")
	(cl-readme:*tab-width* 8)
	(cl-readme:*get-heading-class*
	 (lambda(level) (format nil "header-class-~a" level)))
	(cl-readme:*get-toc-container-class*
	 (lambda(level) (format nil "toc-container-class-~a" level)))
	(cl-readme:*get-toc-item-class*
	 (lambda(level) (format nil "toc-item-class-~a" level))))
    (with-open-file (fh (cl-readme:make-path "docs/index.html")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf-8)
      (cl-readme:doc-to-html fh (get-readme)))
    (with-open-file (fh (cl-readme:make-path "docs/patches.html")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf-8)
	(cl-readme:doc-to-html fh (get-patches))))
  "DONE")

;;(make-readme)

