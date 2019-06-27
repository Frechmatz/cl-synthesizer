(in-package :cl-synthesizer-makedoc)

(defun write-html ()
  (let ((cl-readme:*home-directory* "/Users/olli/src/lisp/cl-synthesizer/")
	(cl-readme:*tab-width* 8))
    (let ((docstr (concatenate
		   'string
		   "<html>"
		   "<head>"
		   "<link rel=\"stylesheet\" href=\"//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic\">"
		   "<link rel=\"stylesheet\" href=\"//cdn.rawgit.com/necolas/normalize.css/master/normalize.css\">"
		   "<link rel=\"stylesheet\" href=\"//cdn.rawgit.com/milligram/milligram/master/dist/milligram.min.css\">"
		   "</head>"
		   "<body>"
		   "<section class=\"container\">"
		   "<h1>cl-synthesizer</h1>"
		   "<p>A Modular Audio Synthesizer library implemented in Common Lisp.</p>"
		   (cl-readme:example-code "src/synthesizer/rack/example-1.lisp")
		   "<h2>Installation</h2>"
		   (cl-readme:read-text-file "makedoc/installation.html")
		   "</section>"
		   "<nav class=\"container\">"
		   "<h2>API Reference</h2>"
		   ;; Table of content in Github flavored markdown
		   ;; https://gist.github.com/asabaylus/3071099
		   "<ul>"
		   "<li><a href=\"#environment\">Environment</a></li>"
		   "<li><a href=\"#rack\">Rack</a></li>"
		   "<li><a href=\"#modules\">Modules</a>"
		   "<ul>"
		   "<li><a href=\"#vco\">VCO</a></li>"
		   "<li><a href=\"#vca\">VCA</a></li>"
		   "<li><a href=\"#adsr\">ADSR</a></li>"
		   "<li><a href=\"#multiple\">Multiple</a></li>"
		   "<li><a href=\"#midi-interface\">MIDI Interface</a></li>"
		   "<li><a href=\"#midi-cc-interface\">MIDI CC Interface</a></li>"
		   "<li><a href=\"#midi-sequencer\">MIDI Sequencer</a></li>"
		   "<li><a href=\"#fixed-output\">Fixed Output</a></li>"
		   "<li><a href=\"#adder\">Adder</a></li>"
		   "<li><a href=\"#mixer\">Mixer</a></li>"
		   "<li><a href=\"#trigger\">Trigger</a></li>"
		   "<li><a href=\"#ramp\">Ramp</a></li>"
		   "<li><a href=\"#sustain\">Sustain</a></li>"
		   "<li><a href=\"#wave-file-writer\">Wave File Writer</a></li>"
		   "<li><a href=\"#csv-file-writer\">CSV File Writer</a></li>"
		   "</ul>"
		   "</li>"
		   "<li><a href=\"#monitor\">Monitor</a></li>"
		   "<li><a href=\"#midi\">MIDI</a>"
		   "<ul>"
		   "<li><a href=\"#midi-event\">MIDI Event</a></li>"
		   "<li><a href=\"#midi-utilities\">MIDI Utilities</a></li>"
		   "</ul>"
		   "</li>"
		   "<li><a href=\"#conditions\">Conditions</a></li>"
		   "</ul>"
		   "</nav>"
		   "<section class=\"container\">"
		   "<h3 id=\"environment\">Environment</h3>"
		   (cl-readme:make-function-string 'cl-synthesizer:make-environment :append-separator nil)
		   "</section>"
		   "<section class=\"container\">"
		   "<h3 id=\"rack\">Rack</h3>"
		   (cl-readme:make-function-string 'cl-synthesizer:make-rack)
		   (cl-readme:make-function-string 'cl-synthesizer:is-rack)
		   (cl-readme:make-function-string 'cl-synthesizer:add-module)
		   (cl-readme:make-function-string 'cl-synthesizer:add-patch)
		   (cl-readme:make-function-string 'cl-synthesizer:get-module)
		   (cl-readme:make-function-string 'cl-synthesizer:get-module-name)
		   (cl-readme:make-function-string 'cl-synthesizer:find-module)
		   (cl-readme:make-function-string 'cl-synthesizer:get-patches)
		   (cl-readme:make-function-string 'cl-synthesizer:get-modules)
		   (cl-readme:make-function-string 'cl-synthesizer:play-rack)
		   (cl-readme:make-function-string 'cl-synthesizer:get-environment)
		   (cl-readme:make-function-string 'cl-synthesizer:add-hook :append-separator nil)
		   "</section>"
		   "<section class=\"container\">"
		   "<h3 id=\"modules\">Modules</h3>"
		   "<h4 id=\"vco\">VCO</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-modules-vco:make-module :append-separator nil)
		   (cl-readme:example-code "src/modules/vco/example-1.lisp")
		   "<h4 id=\"vca\">VCA</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-modules-vca:make-module :append-separator nil)
		   (cl-readme:example-code "src/modules/vca/example-1.lisp")
		   "<h4 id=\"adsr\">ADSR</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-modules-adsr:make-module :append-separator nil)
		   (cl-readme:example-code "src/modules/adsr/example-1.lisp")
		   "<h4 id=\"multiple\">Multiple</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-modules-multiple:make-module :append-separator nil)
		   (cl-readme:example-code "src/modules/multiple/example-1.lisp")
		   "<h4 id=\"midi-interface\">MIDI Interface</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-modules-midi-interface:make-module :append-separator t)
		   "<h4 id=\"midi-cc-interface\">MIDI CC Interface</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-modules-midi-cc-interface:make-module :append-separator nil)
		   (cl-readme:example-code "src/modules/midi-cc-interface/example-1.lisp")
		   "<h4 id=\"midi-sequencer\">MIDI Sequencer</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-modules-midi-sequencer:make-module :append-separator nil)
		   (cl-readme:example-code "src/modules/midi-sequencer/example-1.lisp")
		   "<h4 id=\"fixed-output\">Fixed Output</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-modules-fixed-output:make-module :append-separator nil)
		   (cl-readme:example-code "src/modules/fixed-output/example-1.lisp")
		   "<h4 id=\"adder\">Adder</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-modules-adder:make-module :append-separator nil)
		   "<h4 id=\"mixer\">Mixer</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-modules-mixer:make-module :append-separator nil)
		   (cl-readme:example-code "src/modules/mixer/example-1.lisp")
		   "<h4 id=\"trigger\">Trigger</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-modules-trigger:make-module :append-separator nil)
		   (cl-readme:example-code "src/modules/trigger/example-1.lisp")
		   "<h4 id=\"ramp\">Ramp</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-modules-ramp:make-module :append-separator nil)
		   (cl-readme:example-code "src/modules/ramp/example-1.lisp")
		   "<h4 id=\"sustain\">Sustain</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-modules-sustain:make-module :append-separator nil)
		   (cl-readme:example-code "src/modules/sustain/example-1.lisp")
		   "<h4 id=\"wave-file-writer\">Wave File Writer</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-modules-wave-file-writer:make-module :append-separator nil)
		   "<h4 id=\"csv-file-writer\">CSV File Writer</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-modules-csv-file-writer:make-module :append-separator nil)
		   "</section>"
		   "<section class=\"container\">"
		   "<h3 id=\"monitor\">Monitor</h3>"
		   (cl-readme:make-function-string 'cl-synthesizer-monitor:add-monitor :append-separator nil)
		   (cl-readme:example-code "src/monitor/monitor/example-1.lisp")
		   (cl-readme:make-function-string 'cl-synthesizer-monitor-wave-handler:make-handler)
		   (cl-readme:make-function-string 'cl-synthesizer-monitor-csv-handler:make-handler :append-separator nil)
		   "</section>"
		   "<section class=\"container\">"
		   "<h3 id=\"midi\">MIDI</h3>"
		   "<h4 id=\"midi-event\">MIDI Event</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-midi-event:make-control-change-event)
		   (cl-readme:make-function-string 'cl-synthesizer-midi-event:make-note-on-event)
		   (cl-readme:make-function-string 'cl-synthesizer-midi-event:make-note-off-event)
		   (cl-readme:make-function-string 'cl-synthesizer-midi-event:control-change-eventp)
		   (cl-readme:make-function-string 'cl-synthesizer-midi-event:note-on-eventp)
		   (cl-readme:make-function-string 'cl-synthesizer-midi-event:note-off-eventp)
		   (cl-readme:make-function-string 'cl-synthesizer-midi-event:get-channel)
		   (cl-readme:make-function-string 'cl-synthesizer-midi-event:get-controller-number)
		   (cl-readme:make-function-string 'cl-synthesizer-midi-event:get-controller-value)
		   (cl-readme:make-function-string 'cl-synthesizer-midi-event:get-note-number)
		   (cl-readme:make-function-string 'cl-synthesizer-midi-event:get-velocity :append-separator nil)
		   "<h4 id=\"midi-utilities\">MIDI Utilities</h4>"
		   (cl-readme:make-function-string 'cl-synthesizer-midi:get-note-number-frequency :append-separator nil)
		   "</section>"
		   "<section class=\"container\">"
		   "<h3 id=\"conditions\">Conditions</h3>"
		   (cl-readme:make-condition-string 'cl-synthesizer:assembly-error :append-separator nil)
		   ;;(documentation 'cl-synthesizer:assembly-error 'type)
		   "</section>"
		   "<section class=\"container\">"
		   (cl-readme:read-text-file "makedoc/acknowledge.html")
		   "</section>"
		   "<footer class=\"container\">"
		   "<hr/><p><small>Generated " (cl-readme:current-date) "</small></p>"
		   "</footer>"
		   "</body></html>")))
      (with-open-file (fh (cl-readme:make-path "docs/index.html")
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :external-format :utf-8)
	(format fh "~a" docstr)))))

;;(write-html)

