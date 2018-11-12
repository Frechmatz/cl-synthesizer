(in-package :cl-synthesizer-makedoc)

(defun write-html ()
    (let ((docstr (concatenate
		   'string
		   "<html><body>"
		   "<h1>cl-synthesizer</h1>"
		   "A Modular Audio Synthesizer library implemented in Common Lisp."
		   (example-code "src/synthesizer/rack/example-1.lisp")
		   "<h2>Installation</h2>"
		   (read-text-file "makedoc/installation.html")
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
		   "<li><a href=\"#envelope\">Envelope</a></li>"
		   "<li><a href=\"#multiple\">Multiple</a></li>"
		   "<li><a href=\"#midi-interface\">MIDI Interface</a></li>"
		   "<li><a href=\"#midi-cc-interface\">MIDI CC Interface</a></li>"
		   "<li><a href=\"#midi-sequencer\">MIDI Sequencer</a></li>"
		   "<li><a href=\"#fixed-output\">Fixed Output</a></li>"
		   "<li><a href=\"#adder\">Adder</a></li>"
		   "<li><a href=\"#mixer\">Mixer</a></li>"
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
		   "<h3 id=\"environment\">Environment</h3>"
		   (make-function-string 'cl-synthesizer:make-environment :append-separator nil)
		   "<h3 id=\"rack\">Rack</h3>"
		   (make-function-string 'cl-synthesizer:make-rack)
		   (make-function-string 'cl-synthesizer:add-module)
		   (make-function-string 'cl-synthesizer:add-patch)
		   (make-function-string 'cl-synthesizer:play-rack)
		   (make-function-string 'cl-synthesizer:get-environment)
		   (make-function-string 'cl-synthesizer:get-module)
		   (make-function-string 'cl-synthesizer:get-patch)
		   (make-function-string 'cl-synthesizer:add-hook :append-separator nil)
		   "<h3 id=\"modules\">Modules</h3>"
		   "<h4 id=\"vco\">VCO</h4>"
		   (make-function-string 'cl-synthesizer-modules-vco:make-module-base)
		   (make-function-string 'cl-synthesizer-modules-vco:make-linear-module :append-separator nil)
		   (example-code "src/modules/vco/example-1.lisp")
		   (make-function-string 'cl-synthesizer-modules-vco:make-exponential-module)
		   "<h4 id=\"vca\">VCA</h4>"
		   (make-function-string 'cl-synthesizer-modules-vca:make-module :append-separator nil)
		   (example-code "src/modules/vca/example-1.lisp")
		   "<h4 id=\"envelope\">Envelope</h4>"
		   (make-function-string 'cl-synthesizer-modules-envelope:make-module :append-separator nil)
		   (example-code "src/modules/envelope/example-1.lisp")
		   "<h4 id=\"multiple\">Multiple</h4>"
		   (make-function-string 'cl-synthesizer-modules-multiple:make-module :append-separator nil)
		   (example-code "src/modules/multiple/example-1.lisp")
		   "<h4 id=\"midi-interface\">MIDI Interface</h4>"
		   (make-function-string 'cl-synthesizer-modules-midi-interface:make-module :append-separator nil)
		   (example-code "src/modules/midi-interface/example-1.lisp")
		   "<h4 id=\"midi-cc-interface\">MIDI CC Interface</h4>"
		   (make-function-string 'cl-synthesizer-modules-midi-cc-interface:make-module :append-separator nil)
		   (example-code "src/modules/midi-cc-interface/example-1.lisp")
		   "<h4 id=\"midi-sequencer\">MIDI Sequencer</h4>"
		   (make-function-string 'cl-synthesizer-modules-midi-sequencer:make-module :append-separator nil)
		   (example-code "src/modules/midi-sequencer/example-1.lisp")
		   "<h4 id=\"fixed-output\">Fixed Output</h4>"
		   (make-function-string 'cl-synthesizer-modules-fixed-output:make-module :append-separator nil)
		   (example-code "src/modules/fixed-output/example-1.lisp")
		   "<h4 id=\"adder\">Adder</h4>"
		   (make-function-string 'cl-synthesizer-modules-adder:make-module :append-separator nil)
		   "<h4 id=\"mixer\">Mixer</h4>"
		   (make-function-string 'cl-synthesizer-modules-mixer:make-module :append-separator nil)
		   (example-code "src/modules/mixer/example-1.lisp")
		   "<h3 id=\"monitor\">Monitor</h3>"
		   (make-function-string 'cl-synthesizer-monitor:add-monitor :append-separator nil)
		   (example-code "src/monitor/monitor/example-1.lisp")
		   (make-function-string 'cl-synthesizer-monitor-wave-handler:wave-file-handler :append-separator nil)
		   "<h3 id=\"midi\">MIDI</h3>"
		   "<h4 id=\"midi-event\">MIDI Event</h4>"
		   (make-function-string 'cl-synthesizer-midi-event:make-control-change-event)
		   (make-function-string 'cl-synthesizer-midi-event:make-note-on-event)
		   (make-function-string 'cl-synthesizer-midi-event:make-note-off-event)
		   (make-function-string 'cl-synthesizer-midi-event:control-change-eventp)
		   (make-function-string 'cl-synthesizer-midi-event:note-on-eventp)
		   (make-function-string 'cl-synthesizer-midi-event:note-off-eventp)
		   (make-function-string 'cl-synthesizer-midi-event:get-channel)
		   (make-function-string 'cl-synthesizer-midi-event:get-controller-number)
		   (make-function-string 'cl-synthesizer-midi-event:get-controller-value)
		   (make-function-string 'cl-synthesizer-midi-event:get-note-number)
		   (make-function-string 'cl-synthesizer-midi-event:get-velocity :append-separator nil)
		   "<h4 id=\"midi-utilities\">MIDI Utilities</h4>"
		   (make-function-string 'cl-synthesizer-midi:get-note-number-frequency :append-separator nil)
		   "<h3 id=\"conditions\">Conditions</h3>"
		   (make-condition-string 'cl-synthesizer:assembly-error :append-separator nil)
		   ;;(documentation 'cl-synthesizer:assembly-error 'type)
		   "<hr/><p><small>Generated " (current-date) "</small></p>"
		   "</body></html>"
		   )))
      (with-open-file (fh (make-path "makedoc/generated/readme.html")
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :external-format :utf-8)
	(format fh "~a" docstr))))

;;(write-html)

