(in-package :cl-synthesizer-makedoc)

(defun get-doc ()
  (let ((tree
	 `("<html>"
	   "<head>"
	   "<link rel=\"stylesheet\" href=\"//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic\">"
	   "<link rel=\"stylesheet\" href=\"//cdn.rawgit.com/necolas/normalize.css/master/normalize.css\">"
	   "<link rel=\"stylesheet\" href=\"//cdn.rawgit.com/milligram/milligram/master/dist/milligram.min.css\">"
	   "</head>"
	   "<body>"
	   "<section class=\"container\">"
	   (section
	    (:name "cl-synthesizer")
	    "<p>A Modular Audio Synthesizer library implemented in Common Lisp. "
	    "The source code can be found <a href=\"https://github.com/Frechmatz/cl-synthesizer\">here.</a>"
	    "</p>"
	    "<p><b>A simple example:</b></p>"
	    ,(cl-readme:example-code "src/synthesizer/rack/example-sine.lisp" :omit-header t)
	    "<p><b>A more complex example:</b></p>"
	    ,(cl-readme:example-code "src/synthesizer/rack/example-voice.lisp" :omit-header t))
	   (section
	    (:name "Installation")
	    ,(cl-readme:read-text-file "makedoc/installation.html")
	    "</section>")
	   "<nav class=\"container\">"
	   (section
	    (:name "API Reference")
	    TOC
	    "</nav>"
	    "<section class=\"container\">"
	    (section (:toc :item :name "Environment")
		     ,(cl-readme:make-function-string 'cl-synthesizer:make-environment :append-separator nil))
	    "</section>"
	    "<section class=\"container\">"
	    (section (:toc :item :name "Rack"))
	    ,(cl-readme:make-function-string 'cl-synthesizer:make-rack)
	    ,(cl-readme:make-function-string 'cl-synthesizer:is-rack)
	    ,(cl-readme:make-function-string 'cl-synthesizer:add-module)
	    ,(cl-readme:make-function-string 'cl-synthesizer:add-patch)
	    ,(cl-readme:make-function-string 'cl-synthesizer:get-module)
	    ,(cl-readme:make-function-string 'cl-synthesizer:get-module-name)
	    ,(cl-readme:make-function-string 'cl-synthesizer:find-module)
	    ,(cl-readme:make-function-string 'cl-synthesizer:get-patches)
	    ,(cl-readme:make-function-string 'cl-synthesizer:get-modules)
	    ,(cl-readme:make-function-string 'cl-synthesizer:play-rack)
	    ,(cl-readme:make-function-string 'cl-synthesizer:get-environment)
	    ,(cl-readme:make-function-string 'cl-synthesizer:add-hook :append-separator nil)
	    "</section>"
	    "<section class=\"container\">"
	    (section
	     (:toc :section :name "Modules")
	     (section
	      (:toc :item :name "VCO")
	      ,(cl-readme:make-function-string 'cl-synthesizer-modules-vco:make-module :append-separator nil)
	      ,(cl-readme:example-code "src/modules/vco/example-1.lisp"))
	     (section
	      (:toc :item :name "VCA")
	      ,(cl-readme:make-function-string 'cl-synthesizer-modules-vca:make-module :append-separator nil)
	      ,(cl-readme:example-code "src/modules/vca/example-1.lisp"))
	     (section
	      (:toc :item :name "ADSR")
	      ,(cl-readme:make-function-string 'cl-synthesizer-modules-adsr:make-module :append-separator nil)
	      ,(cl-readme:example-code "src/modules/adsr/example-1.lisp"))
	     (section
	      (:toc :item :name "Multiple")
	      ,(cl-readme:make-function-string 'cl-synthesizer-modules-multiple:make-module :append-separator nil)
	      ,(cl-readme:example-code "src/modules/multiple/example-1.lisp"))
	     (section
	      (:toc :item :name "MIDI Interface")
	      ,(cl-readme:make-function-string 'cl-synthesizer-modules-midi-interface:make-module :append-separator t))
	     (section
	      (:toc :item :name "MIDI CC Interface")
	      ,(cl-readme:make-function-string 'cl-synthesizer-modules-midi-cc-interface:make-module :append-separator nil)
	      ,(cl-readme:example-code "src/modules/midi-cc-interface/example-1.lisp"))
	     (section
	      (:toc :item :name "MIDI Sequencer")
	      ,(cl-readme:make-function-string 'cl-synthesizer-modules-midi-sequencer:make-module :append-separator nil)
	      ,(cl-readme:example-code "src/modules/midi-sequencer/example-1.lisp"))
	     (section
	      (:toc :item :name "Fixed Output")
	      ,(cl-readme:make-function-string 'cl-synthesizer-modules-fixed-output:make-module :append-separator nil)
	      ,(cl-readme:example-code "src/modules/fixed-output/example-1.lisp"))
	     (section
	      (:toc :item :name "Adder")
	      ,(cl-readme:make-function-string 'cl-synthesizer-modules-adder:make-module :append-separator nil))
	     (section
	      (:toc :item :name "Mixer")
	      ,(cl-readme:make-function-string 'cl-synthesizer-modules-mixer:make-module :append-separator nil)
	      ,(cl-readme:example-code "src/modules/mixer/example-1.lisp"))
	     (section
	      (:toc :item :name "Trigger")
	      ,(cl-readme:make-function-string 'cl-synthesizer-modules-trigger:make-module :append-separator nil)
	      ,(cl-readme:example-code "src/modules/trigger/example-1.lisp"))
	     (section
	      (:toc :item :name "Ramp")
	      ,(cl-readme:make-function-string 'cl-synthesizer-modules-ramp:make-module :append-separator nil)
	      ,(cl-readme:example-code "src/modules/ramp/example-1.lisp"))
	     (section
	      (:toc :item :name "Sustain")
	      ,(cl-readme:make-function-string 'cl-synthesizer-modules-sustain:make-module :append-separator nil)
	      ,(cl-readme:example-code "src/modules/sustain/example-1.lisp"))
	     (section
	      (:toc :item :name "Wave File Writer")
	      ,(cl-readme:make-function-string 'cl-synthesizer-modules-wave-file-writer:make-module :append-separator nil))
	     (section
	      (:toc :item :name "CSV File Writer")
	      ,(cl-readme:make-function-string 'cl-synthesizer-modules-csv-file-writer:make-module :append-separator nil))
	     ) ;; close modules section
	    "</section>"
	    "<section class=\"container\">"
	    (section (:toc :item :name "Monitor"))
	    ,(cl-readme:make-function-string 'cl-synthesizer-monitor:add-monitor :append-separator nil)
	    ,(cl-readme:example-code "src/monitor/monitor/example-1.lisp")
	    ,(cl-readme:make-function-string 'cl-synthesizer-monitor-wave-handler:make-handler)
	    ,(cl-readme:make-function-string 'cl-synthesizer-monitor-csv-handler:make-handler :append-separator nil)
	    "</section>"
	    "<section class=\"container\">"
	    (section
	     (:toc :section :name "MIDI")
	     (section
	      (:toc :item :name "MIDI Event"))
	     ,(cl-readme:make-function-string 'cl-synthesizer-midi-event:make-control-change-event)
	     ,(cl-readme:make-function-string 'cl-synthesizer-midi-event:make-note-on-event)
	     ,(cl-readme:make-function-string 'cl-synthesizer-midi-event:make-note-off-event)
	     ,(cl-readme:make-function-string 'cl-synthesizer-midi-event:control-change-eventp)
	     ,(cl-readme:make-function-string 'cl-synthesizer-midi-event:note-on-eventp)
	     ,(cl-readme:make-function-string 'cl-synthesizer-midi-event:note-off-eventp)
	     ,(cl-readme:make-function-string 'cl-synthesizer-midi-event:get-channel)
	     ,(cl-readme:make-function-string 'cl-synthesizer-midi-event:get-controller-number)
	     ,(cl-readme:make-function-string 'cl-synthesizer-midi-event:get-controller-value)
	     ,(cl-readme:make-function-string 'cl-synthesizer-midi-event:get-note-number)
	     ,(cl-readme:make-function-string 'cl-synthesizer-midi-event:get-velocity :append-separator nil)
	     (section
	      (:toc :item :name "MIDI Utilities")
	      ,(cl-readme:make-function-string 'cl-synthesizer-midi:get-note-number-frequency :append-separator nil))
	     ) ;; close MIDI
	    "</section>"
	    "<section class=\"container\">"
	    (section (:toc :item :name "Conditions"))
	    ,(cl-readme:make-condition-string 'cl-synthesizer:assembly-error :append-separator nil)
	    ;;(documentation 'cl-synthesizer:assembly-error 'type)
	    "</section>")
	   "<section class=\"container\">"
	   (section
	    (:name "Acknowledgements")
	    ,(cl-readme:read-text-file "makedoc/acknowledge.html")
	    "</section>")
	   "<footer class=\"container\">"
	   "<hr/><p><small>Generated " ,(cl-readme:current-date) "</small></p>"
	   "</footer>"
	   "</body></html>")))
    tree))

(defun make-readme ()
  (let ((cl-readme:*home-directory* "/Users/olli/src/lisp/cl-synthesizer/")
	(cl-readme:*tab-width* 8))
    (with-open-file (fh (cl-readme:make-path "docs/index.html")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf-8)
      (cl-readme:doc-to-html fh (get-doc))))
  "DONE")

;;(make-readme)

