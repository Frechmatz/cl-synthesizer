(in-package :cl-synthesizer-make-doc)

;;
;; Helper functions
;;

(defparameter *system-indexes* nil)

(defun get-system-index (system)
  (if (not *system-indexes*)
      (setf *system-indexes* 
	    (list
	     :cl-synthesizer (docparser:parse :cl-synthesizer)
	     :cl-synthesizer/vco (docparser:parse :cl-synthesizer/vco)
	     :cl-synthesizer/multiple (docparser:parse :cl-synthesizer/multiple)
	     :cl-synthesizer/vca (docparser:parse :cl-synthesizer/vca)
	     :cl-synthesizer/midi (docparser:parse :cl-synthesizer/midi)
	     :cl-synthesizer/fixed-output (docparser:parse :cl-synthesizer/fixed-output)
	     :cl-synthesizer/adder (docparser:parse :cl-synthesizer/adder)
	     :cl-synthesizer/mixer (docparser:parse :cl-synthesizer/mixer)
	     :cl-synthesizer/trigger (docparser:parse :cl-synthesizer/trigger)
	     :cl-synthesizer/csv-file-writer (docparser:parse :cl-synthesizer/csv-file-writer)
	     :cl-synthesizer/wave-file-writer (docparser:parse :cl-synthesizer/wave-file-writer)
	     :cl-synthesizer/ramp (docparser:parse :cl-synthesizer/ramp)
	     :cl-synthesizer/sustain (docparser:parse :cl-synthesizer/sustain)
	     :cl-synthesizer/adsr (docparser:parse :cl-synthesizer/adsr)
	     :cl-synthesizer/monitor (docparser:parse :cl-synthesizer/monitor)
	     :cl-synthesizer/monitor-wave-file (docparser:parse :cl-synthesizer/monitor-wave-file)
	     :cl-synthesizer/monitor-csv-file (docparser:parse :cl-synthesizer/monitor-csv-file)
	     :cl-synthesizer/monitor-buffer (docparser:parse :cl-synthesizer/monitor-buffer)
	     :cl-synthesizer/doc (docparser:parse :cl-synthesizer/doc))))
  (let ((system-index (getf *system-indexes* system)))
    (if (not system-index)
	(error "Index for system ~a not found" system))
    system-index))
		  
(defun get-node (index package-name symbol-name)
  (aref (docparser:query
	     index
	     :package-name (string-upcase package-name)
	     :symbol-name (string-upcase symbol-name))
	0))

(defun get-package-docstring (system package-name)
  (let ((docstring nil) (index (get-system-index system)))
    (docparser:do-packages (package index)
      (if (string= (string-upcase package-name) (docparser:package-index-name package))
	  (setf docstring (docparser:package-index-docstring package))))
    (if (not docstring)
	(error "Package ~a not found" package-name))
    docstring))

(defun make-function-string (system package-name symbol-name)
  (let* ((index (get-system-index system))
	 (node (get-node index package-name symbol-name))
	 (lambda-list (docparser:operator-lambda-list node)))
    (concatenate
     'string
     "<b>" package-name ":" (string-downcase symbol-name) "</b>&nbsp;"
     (string-downcase (format nil "~a" (if lambda-list lambda-list "()")))
     "<p>" (docparser:node-docstring node) "</p>")))

(defun make-package-string (system package-name)
  (concatenate
   'string
   "<p>" (get-package-docstring system package-name) "</p>"))
    
(defun make-condition-string (system package-name symbol-name)
  (let* ((index (get-system-index system))
	 (node (get-node index package-name symbol-name)))
 (concatenate
  'string
  "<b>" package-name ":" (string-downcase symbol-name) "</b>"
  "<p>"  (docparser:node-docstring node) "</p>")))

(defun make-variable-string (system package-name symbol-name)
  "Returns HTML representation of a variable"
  (let* ((index (get-system-index system)) (node (get-node index package-name symbol-name)))
    (concatenate
     'string
     "<b>" package-name ":" (string-downcase symbol-name) "</b>"
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

(defun make-patch (system &key package path wave-file (show-code nil))
  (let ((link-id (gensym)) (source-code-id (gensym)))
    (list 'heading (list :name (make-package-string system package) :toc t)
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

(defun get-readme ()
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
				 :name ,(get-package-docstring :cl-synthesizer/doc "cl-synthesizer-patches-saw")))
		       ,(make-code-string "makedoc/patches/saw.lisp")
		       ,(make-audio-element "saw.wav")
		       (heading (:toc t
				 :name ,(get-package-docstring :cl-synthesizer/doc "cl-synthesizer-patches-frequency-modulated-saw")))
		       ,(make-code-string "makedoc/patches/frequency-modulated-saw.lisp")
		       ,(make-audio-element "frequency-modulated-saw.wav")
		       (heading (:toc t
				 :name ,(get-package-docstring :cl-synthesizer/doc "cl-synthesizer-patches-two-frequency-modulated-saws")))
		       ,(make-code-string "makedoc/patches/two-frequency-modulated-saws.lisp")
		       ,(make-audio-element "two-frequency-modulated-saws.wav"))
	      ;; No change log yet. All is work in progress :)
	      ;; (heading (:name "Change-Log" :toc t)
	      ;; 	       (heading (:name "Version 1.0.0")
	      ;; 			"<p>Initial version. Not tagged.</p>")
	      ;; 	       (heading (:name "Version 2.x")
	      ;; 			"Work in progress. Master branch."
	      ;; 			"<ul>"
	      ;; 			"<li>Removed bridge modules.</li>"
	      ;; 			"<li>Added rack:add-rack-input.</li>"
	      ;; 			"<li>Added rack:add-rack-output.</li>"
	      ;; 			"<li>Added module input socket getters.</li>"
	      ;; 			"</ul>"))
	      (heading (:name "Concepts" :toc t)
		       (heading (:name "Environment" :toc t)
				,(cl-html-readme:read-file "makedoc/environment-introduction.html"))
		       (heading (:name "Module" :toc t)
				,(cl-html-readme:read-file "makedoc/module-introduction.html")
				(heading (:name "Example: A module" :toc t)
					 ,(make-code-string "src/synthesizer/examples/example-1/adder-2.lisp"))
				(heading (:name "Example: A module based on other modules" :toc t)
					 ,(make-code-string "src/synthesizer/examples/example-1/adder-4.lisp")))
		       (heading (:name "Rack" :toc t)
				"<p>Racks are holding modules and their connections with each other. The connections are so called \"Patches\".</p>"
				(heading (:name "Example" :toc t)
					 ,(make-code-string "src/synthesizer/examples/example-1/adder-rack.lisp"))))
	      (heading (:name "API" :toc t)
		       (heading (:toc t :name "Environment")
				(heading (:toc t :name "make-environment")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "make-environment"))
				(heading (:toc t :name "*home-directory*")
					 ,(make-variable-string :cl-synthesizer "cl-synthesizer" "*home-directory*")))
		       (heading (:toc t :name "Module")
				(heading (:toc t :name "get-module-name")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "get-module-name"))
				(heading (:toc t :name "get-module-rack")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "get-module-rack"))
				(heading (:toc t :name "update")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "update"))
				(heading (:toc t :name "shutdown")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "shutdown"))
				(heading (:toc t :name "is-rack")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "is-rack")))
		       (heading (:toc t :name "Rack")
				(heading (:toc t :name "make-rack")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "make-rack"))
				(heading (:toc t :name "get-environment")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "get-environment"))
				(heading (:toc t :name "add-module")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "add-module"))
				(heading (:toc t :name "get-module")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "get-module"))
				(heading (:toc t :name "get-modules")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "get-modules"))
				(heading (:toc t :name "add-patch")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "add-patch"))
				(heading (:toc t :name "get-patches")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "get-patches"))
				(heading (:toc t :name "add-rack-input")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "add-rack-input"))
				(heading (:toc t :name "add-rack-output")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "add-rack-output"))
				(heading (:toc t :name "add-hook")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "add-hook"))
				(heading (:toc t :name "play-rack")
					 ,(make-function-string :cl-synthesizer "cl-synthesizer" "play-rack")))
		       (heading	(:toc t :name "Modules")
			(heading (:toc t :name "VCO")
				 ,(make-function-string :cl-synthesizer/vco "cl-synthesizer-modules-vco" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/vco/example-1.lisp"))
			(heading (:toc t :name "VCA")
				 ,(make-function-string :cl-synthesizer/vca "cl-synthesizer-modules-vca" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/vca/example-1.lisp"))
			(heading (:toc t :name "ADSR")
				 ,(make-function-string :cl-synthesizer/adsr "cl-synthesizer-modules-adsr" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/adsr/example-1.lisp"))
			(heading (:toc t :name "Multiple")
				 ,(make-function-string :cl-synthesizer/multiple "cl-synthesizer-modules-multiple" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/multiple/example-1.lisp"))
			(heading (:toc t :name "Fixed Output")
				 ,(make-function-string :cl-synthesizer/fixed-output "cl-synthesizer-modules-fixed-output" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/fixed-output/example-1.lisp"))
			(heading (:toc t :name "Adder")
				 ,(make-function-string :cl-synthesizer/adder "cl-synthesizer-modules-adder" "make-module"))
			(heading (:toc t :name "Mixer")
				 ,(make-function-string :cl-synthesizer/mixer "cl-synthesizer-modules-mixer" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/mixer/example-1.lisp"))
			(heading (:toc t :name "Trigger")
				 ,(make-function-string :cl-synthesizer/trigger "cl-synthesizer-modules-trigger" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/trigger/example-1.lisp"))
			(heading (:toc t :name "Ramp")
				 ,(make-function-string :cl-synthesizer/ramp "cl-synthesizer-modules-ramp" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/ramp/example-1.lisp"))
			(heading (:toc t :name "Sustain")
				 ,(make-function-string :cl-synthesizer/sustain "cl-synthesizer-modules-sustain" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/sustain/example-1.lisp"))
			(heading (:toc t :name "Wave File Writer")
				 ,(make-function-string :cl-synthesizer/wave-file-writer "cl-synthesizer-modules-wave-file-writer" "make-module"))
			(heading (:toc t :name "CSV File Writer")
				 ,(make-function-string :cl-synthesizer/csv-file-writer "cl-synthesizer-modules-csv-file-writer" "make-module")))
		       (heading (:toc t :name "Monitors")
				,(cl-html-readme:read-file "makedoc/monitor-introduction.html")
				(heading (:toc t :name "add-monitor")
						  ,(make-function-string :cl-synthesizer/monitor "cl-synthesizer-monitor" "add-monitor"))
					 (heading (:toc t :name "wave-file-agent") ,(make-function-string :cl-synthesizer/monitor-wave-file "cl-synthesizer-monitor-wave-file-agent" "make-backend")
						  ,(make-example-header)
						  ,(make-code-string "src/monitor/wave-file/example-1.lisp"))
					 (heading (:toc t :name "csv-file-agent")
						  ,(make-function-string :cl-synthesizer/monitor-csv-file "cl-synthesizer-monitor-csv-file-agent" "make-backend")
						  ,(make-example-header)
						  ,(make-code-string "src/monitor/csv-file/example-1.lisp"))
					 (heading (:toc t :name "buffer-agent")
						  ,(make-function-string :cl-synthesizer/monitor-buffer "cl-synthesizer-monitor-buffer-agent" "make-backend")
						  ,(make-example-header)
						  ,(make-code-string "src/monitor/buffer/example-1.lisp")))
		       (heading (:toc t :name "MIDI")
				(heading (:toc t :name "MIDI Event")
				,(make-function-string :cl-synthesizer/midi "cl-synthesizer-midi-event" "make-control-change-event")
				,(make-function-string :cl-synthesizer/midi "cl-synthesizer-midi-event" "make-note-on-event")
				,(make-function-string :cl-synthesizer/midi "cl-synthesizer-midi-event" "make-note-off-event")
				,(make-function-string :cl-synthesizer/midi "cl-synthesizer-midi-event" "control-change-eventp")
				,(make-function-string :cl-synthesizer/midi "cl-synthesizer-midi-event" "note-on-eventp")
				,(make-function-string :cl-synthesizer/midi "cl-synthesizer-midi-event" "note-off-eventp")
				,(make-function-string :cl-synthesizer/midi "cl-synthesizer-midi-event" "get-channel")
				,(make-function-string :cl-synthesizer/midi "cl-synthesizer-midi-event" "get-controller-number")
				,(make-function-string :cl-synthesizer/midi "cl-synthesizer-midi-event" "get-control-value")
				,(make-function-string :cl-synthesizer/midi "cl-synthesizer-midi-event" "get-note-number")
				,(make-function-string :cl-synthesizer/midi "cl-synthesizer-midi-event" "get-velocity"))
  			(heading (:toc t :name "MIDI Polyphonic Interface")
				 ,(make-function-string :cl-synthesizer/midi "cl-synthesizer-modules-midi-polyphonic-interface" "make-module"))
			(heading (:toc t :name "MIDI Monophonic Interface")
				 ,(make-function-string :cl-synthesizer/midi "cl-synthesizer-modules-midi-monophonic-interface" "make-module"))
			(heading (:toc t :name "MIDI Relative CC Interface")
				 ,(make-function-string :cl-synthesizer/midi "cl-synthesizer-modules-midi-relative-cc-interface" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/midi/modules/midi-relative-cc-interface/example-1.lisp"))
			(heading (:toc t :name "MIDI Sequencer")
				 ,(make-function-string :cl-synthesizer/midi "cl-synthesizer-modules-midi-sequencer" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/midi/modules/midi-sequencer/example-1.lisp"))
			(heading (:toc t :name "MIDI Utilities")
					 ,(make-function-string :cl-synthesizer/midi "cl-synthesizer-midi" "get-note-number-frequency")
					 ,(make-note-number-chart)))
		       (heading (:toc t :name "Conditions")
				,(make-condition-string :cl-synthesizer "cl-synthesizer" "assembly-error")))
	      (heading (:name "Run tests" :toc t)
		       "<pre><code>(asdf:test-system :cl-synthesizer)</code></pre>")
	      (heading (:name "Run examples" :toc t)
		       "Run all examples of cl-synthesizer. Generated files are written to <code>~/cl-synthesizer-examples/</code>" 
		       ,(make-code-string "makedoc/run-examples.lisp"))
	      (heading (:name "Run profiler" :toc t)
		       "Run the profiling suite. Generated files files are written to <code>~/cl-synthesizer-profiler/</code>" 
		       ,(make-code-string "makedoc/run-profiler.lisp"))
	      (heading (:name "Generate documentation" :toc t)
		       "Depends on :cl-html-readme and :docparser which are available via Quicklisp."
		       ,(make-code-string "makedoc/generate-doc.lisp"))
	      (heading (:name "Acknowledgements" :toc t)
		       "<p>Envelope generation has been inspired by <a href=\"https://github.com/dhemery/DHE-Modules/wiki/Multi-Stage-Envelopes\">dhemery</a></p>"))
    (semantic (:name "footer")
	      "<hr/><p><small>Generated " ,(now) "</small></p>")
    "</body></html>"))

;;
;; Patches
;;

(defun get-patches ()
  `("<html><head>"
    "<script type=\"text/javascript\" src=\"toggledisplay.js\"></script>"
    "<link href=\"styles.css\" rel=\"stylesheet\" type=\"text/css\"/>"
    "</head><body>"
    (semantic (:name "header")
	      (heading (:name "cl-synthesizer-patches")
		       ,(cl-html-readme:read-file "makedoc/patches-introduction.html")))
    (semantic (:name "section")
	      ,(make-patch :cl-synthesizer/doc
			   :package "cl-synthesizer-patches-saw"
			   :path "makedoc/patches/saw.lisp"
			   :wave-file "saw.wav")
	      ,(make-patch :cl-synthesizer/doc
			   :package "cl-synthesizer-patches-frequency-modulated-saw"
			   :path "makedoc/patches/frequency-modulated-saw.lisp"
			   :wave-file "frequency-modulated-saw.wav")
	      ,(make-patch :cl-synthesizer/doc
			   :package "cl-synthesizer-patches-two-frequency-modulated-saws"
			   :path "makedoc/patches/two-frequency-modulated-saws.lisp"
			   :wave-file "two-frequency-modulated-saws.wav"))
    (semantic (:name "footer")
	      "<hr/><p><small>Generated " ,(now) "</small></p>")
    "</body></html>"))

;;
;; Generate index.html, patches.html
;;

(defun make-doc ()
  (let ((cl-synthesizer:*home-directory* (asdf:system-source-directory :cl-synthesizer/doc)))
    ;; Generate wave files of example patches
    (cl-synthesizer-patches-saw::run-example)
    (cl-synthesizer-patches-frequency-modulated-saw::run-example)
    (cl-synthesizer-patches-two-frequency-modulated-saws::run-example))
  ;; Generate html files
  (let ((cl-html-readme:*home-directory* (asdf:system-source-directory :cl-synthesizer/doc))
	(cl-html-readme:*tab-width* 8)) // 8 == default emacs column width
    (with-open-file (fh (cl-html-readme:make-path "docs/index.html")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf-8)
      (cl-html-readme:doc-to-html fh (get-readme)))
    (with-open-file (fh (cl-html-readme:make-path "docs/patches.html")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf-8)
      (cl-html-readme:doc-to-html fh (get-patches))))
  "DONE")

;;(make-doc)


