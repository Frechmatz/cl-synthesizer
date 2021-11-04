(in-package :cl-synthesizer-make-doc)

;;
;; Helper functions
;;

(defun make-index (system)
  (docparser:parse system))

(defun get-node (index package-name symbol-name)
  (aref (docparser:query
	     index
	     :package-name (string-upcase package-name)
	     :symbol-name (string-upcase symbol-name))
	0))

(defun get-package-docstring (index package-name)
  (let ((docstring nil))
    (docparser:do-packages (package index)
      (if (string= (string-upcase package-name) (docparser:package-index-name package))
	  (setf docstring (docparser:package-index-docstring package))))
    (if (not docstring)
	(error "Package ~a not found" package-name))
    docstring))

(defun make-function-string (index package-name symbol-name)
  (let* ((node (get-node index package-name symbol-name))
	 (lambda-list (docparser:operator-lambda-list node)))
    (concatenate
     'string
     "<b>" (string-downcase symbol-name) "</b>&nbsp;"
     (string-downcase (format nil "~a" (if lambda-list lambda-list "()")))
     "<p>" (docparser:node-docstring node) "</p>")))

(defun make-package-string (index package-name)
  (concatenate
   'string
   "<p>" (get-package-docstring index package-name) "</p>"))

(defun make-condition-string (index package-name symbol-name)
  (let* ((node (get-node index package-name symbol-name)))
 (concatenate
  'string
  "<b>" (string-downcase symbol-name) "</b>"
  "<p>"  (docparser:node-docstring node) "</p>")))

(defun make-variable-string (index package-name symbol-name)
  "Returns HTML representation of a variable"
  (let ((node (get-node index package-name symbol-name)))
    (concatenate
     'string
     "<b>" (string-downcase symbol-name) "</b>"
     "<p>" (docparser:node-docstring node) "</p>")))

(defun make-code-string (path)
  (concatenate
   'string
   "<p><pre><code>"
   (cl-html-readme:read-file path :replace-tabs t :escape t)
   "</code></pre></p>"))

(defun make-example-header (&key (title nil))
  (concatenate
   'string
   "<p><b>" (if title title "Example") "</b></p>"))
      
(defun make-audio-element (filename)
  (concatenate
   'string
   "<p><audio controls preload=\"none\">" ;; loop=\"false\">"
   "Your browser does not support the <code>audio</code> element."
   "<source src=\""
   ;; Add query parameter with timestamp as a cache buster
   (format nil "~a?cb=~a" filename (get-universal-time))
   "\" type=\"audio/wav\">"
   "</audio></p>"))

(defun make-patch (index &key package path wave-file (show-code nil))
  (let ((link-id (gensym)) (source-code-id (gensym)))
    (list 'heading (list :name (make-package-string index package) :toc t)
	  (make-audio-element wave-file)
	  (format nil "<p><a href=\"#\" id=\"~a\"" link-id)
	  (format nil "onclick=\"return toggleDisplay('~a', '~a', '~a', '~a')\">~a</a></p>"
		  "Show patch"
		  "Hide patch"
		  link-id source-code-id (if show-code "Hide patch" "Show patch"))
	  (format nil
		  "<div style=\"display: ~a\" id='~a'>~a</div>"
		  (if show-code "block" "none")
		  source-code-id
		  (make-code-string path)))))

(defun now ()
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (get-decoded-time)
    (declare (ignore dow dst-p tz))
    ;; 2018-09-19 21:28:16
    (let ((str (format nil "~4,'0d-~2,'0d-~2,'0d  ~2,'0d:~2,'0d:~2,'0d" yr mon day hr min sec)))
      str)))

(defun make-note-number-chart ()
  (let ((chart (make-string-output-stream)))
    (write-line "<table><thead><tr><th>Note number</th><th>Frequency</th></tr></thead><tbody>" chart) 
    (dotimes (note-number 128)
      (write-line 
       (concatenate
	'string
	"<tr>"
	"<td>" (format nil "~a" note-number) "</td>"
	"<td>" (format nil "~a" (cl-synthesizer-midi:get-note-number-frequency note-number)) "</td>"
	"</tr>")
       chart))
    (write-line "</tbody></table>" chart)
    (get-output-stream-string chart)))

;;
;; Readme
;;

(defun get-readme (lib-index readme-index)
  `("<html>"
    "<head><link href=\"styles.css\" rel=\"stylesheet\" type=\"text/css\"/></head>"
    "<body>"
    (semantic (:name "header")
	      (heading
	       (:name "cl-synthesizer")
	       ,(cl-html-readme:read-file "makedoc/introduction.html")))
    (semantic (:name "nav")
	      (heading (:name "Table of contents") (toc)))
    (semantic (:name "section")
	      (heading (:name "Installation" :toc t)
		       ,(cl-html-readme:read-file "makedoc/installation.html"))
	      (heading (:name "Examples" :toc t)
		       (heading (:toc t
				 :name ,(get-package-docstring readme-index "cl-synthesizer-patches-sine")))
		       ,(make-code-string "makedoc/patches/sine.lisp")
		       ,(make-audio-element "sine.wav")
		       (heading (:toc t
				 :name ,(get-package-docstring readme-index "cl-synthesizer-patches-siren")))
		       ,(make-code-string "makedoc/patches/siren.lisp")
		       ,(make-audio-element "siren.wav"))
	      (heading (:name "Concepts" :toc t)
		       (heading (:name "Environment" :toc t)
				,(cl-html-readme:read-file "makedoc/environment-introduction.html")
				,(make-code-string "makedoc/snippets/environment-make-environment.lisp"))
		       (heading (:name "Module" :toc t)
				,(cl-html-readme:read-file "makedoc/module-introduction.html")
				"<p>Define a module:</p>"
				,(make-code-string "makedoc/snippets/module-blueprint.lisp")
				"<p>Add the module to a rack (Racks are explained in the following chapter):</p>"
				,(make-code-string "makedoc/snippets/module-blueprint-add.lisp"))
		       (heading (:name "Rack" :toc t)
				"<p>Racks contain modules and their connections which each other. The connections are so called \"Patches\". A rack is represented by a property list, which is fully compatible with a module. Beside the module functionality a rack provides Module and Patch management, Hooks and Compilation. Racks are instantiated via \"cl-synthesizer:make-rack\"</p>"
				,(make-code-string "makedoc/snippets/rack-make-rack.lisp")
				"<p>After the rack has been created, modules and patches can be added to it.</p>"
				,(make-code-string "makedoc/snippets/rack-add-modules.lisp")
				"<p>The rack is now ready to use and a \"tick\" can be processed. When a tick is to be processed, the rack checks if it has been modified, and if yes, a compilation occurs. The compilation step processes the modules and patch table of the rack in order to eliminate/minimize any expensive property list lookups during the actual execution.</p>"
				,(make-code-string "makedoc/snippets/rack-process-tick.lisp")
				"<p>Lets get the :sine output of the \"VCO\" module. The following snippet illustrates the low-level Api of the synthesizer. The recommended way to retrieve output values is to use a \"Monitor\”. Monitors are explained in the next chapter.</p>"
				,(make-code-string "makedoc/snippets/rack-get-output.lisp")
				"<p>When all things are done the rack should be shut down in order to release any resources that may have been allocated, such as opened files.</p>"
				,(make-code-string "makedoc/snippets/rack-shutdown.lisp")
				"<p>Racks may expose input and output sockets. When a rack exposes sockets it can be used like any other module when populating another rack. The inputs and outputs are accessible by the internal patching of the rack via so called \"Bridge modules\". \"INPUT\" provides the inputs, and \"OUTPUT\" the outputs of the rack.</p>"
				,(make-code-string "makedoc/snippets/rack-expose-sockets.lisp")
				"<p>A more comprehensive example can be found <a href=\"https://github.com/Frechmatz/cl-synthesizer/blob/master/makedoc/patches/siren.lisp\">here</a>.</p>")
		       (heading (:name "Monitor" :toc t)
				,(cl-html-readme:read-file "makedoc/monitor-introduction.html")
				,(make-example-header)
				,(make-code-string "src/monitor/example-1.lisp")))
	      (heading (:name "API" :toc t)
		       (heading (:toc t :name "Environment")
				(heading (:toc t :name "make-environment")
					 ,(make-function-string lib-index "cl-synthesizer" "make-environment"))
				(heading (:toc t :name "*home-directory*")
					 ,(make-variable-string lib-index "cl-synthesizer" "*home-directory*")))
		       (heading (:toc t :name "Rack")
				(heading (:toc t :name "make-rack")
					 ,(make-function-string lib-index "cl-synthesizer" "make-rack"))
				(heading (:toc t :name "add-module")
					 ,(make-function-string lib-index "cl-synthesizer" "add-module"))
				(heading (:toc t :name "add-patch")
					 ,(make-function-string lib-index "cl-synthesizer" "add-patch"))
				(heading (:toc t :name "get-module")
					 ,(make-function-string lib-index "cl-synthesizer" "get-module"))
				(heading (:toc t :name "get-module-name")
					 ,(make-function-string lib-index "cl-synthesizer" "get-module-name"))
				(heading (:toc t :name "find-module")
					 ,(make-function-string lib-index "cl-synthesizer" "find-module"))
				(heading (:toc t :name "get-patches")
					 ,(make-function-string lib-index "cl-synthesizer" "get-patches"))
				(heading (:toc t :name "get-modules")
					 ,(make-function-string lib-index "cl-synthesizer" "get-modules"))
				(heading (:toc t :name "add-hook")
					 ,(make-function-string lib-index "cl-synthesizer" "add-hook"))
				(heading (:toc t :name "play-rack")
					 ,(make-function-string lib-index "cl-synthesizer" "play-rack"))
				(heading (:toc t :name "is-rack")
					 ,(make-function-string lib-index "cl-synthesizer" "is-rack"))
				(heading (:toc t :name "get-environment")
					 ,(make-function-string lib-index "cl-synthesizer" "get-environment")))
		       (heading	(:toc t :name "Modules")
			(heading (:toc t :name "VCO")
				 ,(make-function-string lib-index "cl-synthesizer-modules-vco" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/vco/example-1.lisp"))
			(heading (:toc t :name "VCA")
				 ,(make-function-string lib-index "cl-synthesizer-modules-vca" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/vca/example-1.lisp"))
			(heading (:toc t :name "ADSR")
				 ,(make-function-string lib-index "cl-synthesizer-modules-adsr" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/adsr/example-1.lisp"))
			(heading (:toc t :name "Multiple")
				 ,(make-function-string lib-index "cl-synthesizer-modules-multiple" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/multiple/example-1.lisp"))
			(heading (:toc t :name "MIDI Polyphonic Interface")
				 ,(make-function-string lib-index "cl-synthesizer-modules-midi-polyphonic-interface" "make-module"))
			(heading (:toc t :name "MIDI Monophonic Interface")
				 ,(make-function-string lib-index "cl-synthesizer-modules-midi-monophonic-interface" "make-module"))
			(heading (:toc t :name "MIDI Relative CC Interface")
				 ,(make-function-string lib-index "cl-synthesizer-modules-midi-relative-cc-interface" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/midi-relative-cc-interface/example-1.lisp"))
			(heading (:toc t :name "MIDI Sequencer")
				 ,(make-function-string lib-index "cl-synthesizer-modules-midi-sequencer" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/midi-sequencer/example-1.lisp"))
			(heading (:toc t :name "Fixed Output")
				 ,(make-function-string lib-index "cl-synthesizer-modules-fixed-output" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/fixed-output/example-1.lisp"))
			(heading (:toc t :name "Adder")
				 ,(make-function-string lib-index "cl-synthesizer-modules-adder" "make-module"))
			(heading (:toc t :name "Mixer")
				 ,(make-function-string lib-index "cl-synthesizer-modules-mixer" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/mixer/example-1.lisp"))
			(heading (:toc t :name "Trigger")
				 ,(make-function-string lib-index "cl-synthesizer-modules-trigger" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/trigger/example-1.lisp"))
			(heading (:toc t :name "Ramp")
				 ,(make-function-string lib-index "cl-synthesizer-modules-ramp" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/ramp/example-1.lisp"))
			(heading (:toc t :name "Sustain")
				 ,(make-function-string lib-index "cl-synthesizer-modules-sustain" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/sustain/example-1.lisp"))
			(heading (:toc t :name "Wave File Writer")
				 ,(make-function-string lib-index "cl-synthesizer-modules-wave-file-writer" "make-module"))
			(heading (:toc t :name "CSV File Writer")
				 ,(make-function-string lib-index "cl-synthesizer-modules-csv-file-writer" "make-module")))
		       (heading (:toc t :name "Monitor")
				(heading (:toc t :name "add-monitor")
					 ,(make-function-string lib-index "cl-synthesizer-monitor" "add-monitor"))
				(heading (:toc t :name "wave-file-agent")
					 ,(make-function-string lib-index "cl-synthesizer-monitor-wave-file-agent" "make-backend"))
				(heading (:toc t :name "csv-file-agent")
					 ,(make-function-string lib-index "cl-synthesizer-monitor-csv-file-agent" "make-backend"))
				(heading (:toc t :name "buffer-agent")
					 ,(make-function-string lib-index "cl-synthesizer-monitor-buffer-agent" "make-backend")))
		       (heading (:toc t :name "MIDI")
				(heading (:toc t :name "MIDI Event"))
				,(make-function-string lib-index "cl-synthesizer-midi-event" "make-control-change-event")
				,(make-function-string lib-index "cl-synthesizer-midi-event" "make-note-on-event")
				,(make-function-string lib-index "cl-synthesizer-midi-event" "make-note-off-event")
				,(make-function-string lib-index "cl-synthesizer-midi-event" "control-change-eventp")
				,(make-function-string lib-index "cl-synthesizer-midi-event" "note-on-eventp")
				,(make-function-string lib-index "cl-synthesizer-midi-event" "note-off-eventp")
				,(make-function-string lib-index "cl-synthesizer-midi-event" "get-channel")
				,(make-function-string lib-index "cl-synthesizer-midi-event" "get-controller-number")
				,(make-function-string lib-index "cl-synthesizer-midi-event" "get-control-value")
				,(make-function-string lib-index "cl-synthesizer-midi-event" "get-note-number")
				,(make-function-string lib-index "cl-synthesizer-midi-event" "get-velocity")
				(heading (:toc t :name "MIDI Utilities")
					 ,(make-function-string lib-index "cl-synthesizer-midi" "get-note-number-frequency")
					 ,(make-note-number-chart)))
		       (heading (:toc t :name "Conditions")
				,(make-condition-string lib-index "cl-synthesizer" "assembly-error")))
	      (heading (:name "Run tests" :toc t)
		       "<pre><code>(asdf:test-system :cl-synthesizer)</code></pre>")
	      (heading (:name "Run examples" :toc t)
		       "Run all examples of cl-synthesizer. Generated files are written into <code>~/cl-synthesizer-examples/</code>" 
		       ,(make-code-string "makedoc/run-examples.lisp"))
	      (heading (:name "Run profiler" :toc t)
		       "Run the profiling suite. Generated files files are written into <code>~/cl-synthesizer-profiler/</code>" 
		       ,(make-code-string "makedoc/run-profiler.lisp"))
	      (heading (:name "Generate documentation" :toc t)
		       ,(make-code-string "makedoc/generate-doc.lisp"))
	      (heading (:name "Acknowledgements" :toc t)
		       ,(cl-html-readme:read-file "makedoc/acknowledge.html")))
    (semantic (:name "footer")
	      "<hr/><p><small>Generated " ,(now) "</small></p>")
    "</body></html>"))

;;
;; Patches
;;

(defun get-patches (readme-index)
  `("<html><head>"
    "<script type=\"text/javascript\" src=\"toggledisplay.js\"></script>"
    "<link href=\"styles.css\" rel=\"stylesheet\" type=\"text/css\"/>"
    "</head><body>"
    (semantic (:name "header")
	      (heading (:name "cl-synthesizer-patches")
		       ,(cl-html-readme:read-file "makedoc/patches-introduction.html")))
    (semantic (:name "section")
	      ,(make-patch readme-index
			   :package "cl-synthesizer-patches-sine"
			   :path "makedoc/patches/sine.lisp"
			   :wave-file "sine.wav")
	      ,(make-patch readme-index
			   :package "cl-synthesizer-patches-siren"
			   :path "makedoc/patches/siren.lisp"
			   :wave-file "siren.wav"))
    (semantic (:name "footer")
	      "<hr/><p><small>Generated " ,(now) "</small></p>")
    "</body></html>"))

;;
;; Generate index.html, patches.html
;;

(defun make-doc ()
  (let ((cl-synthesizer:*home-directory* (asdf:system-source-directory :cl-synthesizer/doc)))
    ;; Generate patches
    (cl-synthesizer-patches-siren::run-example)
    (cl-synthesizer-patches-sine::run-example))
  ;; Generate html files
  (let ((lib-index (make-index :cl-synthesizer))
	(readme-index (make-index :cl-synthesizer/doc)))
    (let ((cl-html-readme:*home-directory* (asdf:system-source-directory :cl-synthesizer/doc))
	  (cl-html-readme:*tab-width* 4))
      (with-open-file (fh (cl-html-readme:make-path "docs/index.html")
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :external-format :utf-8)
	(cl-html-readme:doc-to-html fh (get-readme lib-index readme-index)))
      (with-open-file (fh (cl-html-readme:make-path "docs/patches.html")
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :external-format :utf-8)
	(cl-html-readme:doc-to-html fh (get-patches readme-index)))))
    "DONE")

;;(make-doc)

