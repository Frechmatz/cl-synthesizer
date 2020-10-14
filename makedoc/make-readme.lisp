(in-package :cl-synthesizer-makedoc)


(defun get-readme (lib-index)
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
	       ,(cl-html-readme:read-file "makedoc/introduction.html")
	       ,(cl-html-readme:read-file "makedoc/principles.html"))
	      "<p>The source code of cl-synthesizer can be found <a href=\"https://github.com/Frechmatz/cl-synthesizer\">here</a>.</p>")
    (semantic (:name "nav")
	      (heading (:name "Table of contents") (toc)))
    (semantic (:name "section")
	      (heading (:name "Installation" :toc t)
		       ,(cl-html-readme:read-file "makedoc/installation.html"))
	      (heading (:name "Examples" :toc t)
		       (heading (:toc t :name ,(documentation (find-package 'cl-synthesizer-patches-sine) t))
				,(make-code-string "patches/sine.lisp")
				,(make-audio-element "sine.wav"))
		       (heading (:toc t :name ,(documentation (find-package 'cl-synthesizer-patches-siren) t))
				,(make-code-string "patches/siren.lisp")
				,(make-audio-element "siren.wav")))
	      (heading (:name "Concepts" :toc t)
		       (heading (:name "Environment" :toc t)
				"<p>An environment defines properties such as the sample rate and the home directory of the synthesizer. An environment is represented by a property list.</p>"
				,(make-code-string "makedoc/snippets/environment-make-environment.lisp"))
		       (heading (:name "Module" :toc t)
				"<p>Modules are the heart of cl-synthesizer because they are responsible for producing the actual audio data.</p>
<p>Modules do have a name, inputs, outputs and a shutdown function. The inputs/outputs are represented by keywords and are so called sockets. The shutdown function can be used to release resources 
that have been allocated by the module, for example opened files.</p>
<p>Beside the input/output sockets a module can also provide \"state sockets\".
State sockets expose internal states of the module. These sockets are not accessible when connecting modules with each other. They are meant 
to be a debugging/analysis tool. For example to create a plot of the phase of an oscillator over time, a :phase state socket in conjunction with a Monitor is the way to go.</p>
<p>A module is represented by a property list. This list provides functions such as to get the input sockets, to get the output sockets, to get the state sockets, 
to set input values, to retrieve output values, to update the module, to shutdown the module and so on.</p>
<p>A module must provide a factory/instantiation function. The typical name of this function is \"make-module\". When a module is added to the synthesizer then not the 
    readily instantiated module is passed, but its factory function. This function is called by the synthesizer. The synthesizer passes the module name, 
    the environment and any arbitrary initialization parameters to it.</p>
<p>A module can implement all its logic on its own but it can also use other modules. An example of a module using other modules is the
    <a href=\"https://github.com/Frechmatz/cl-synthesizer/blob/master/src/modules/mixer.lisp\">Mixer</a>.</p>
<p>For each input/output socket that a module exposes, it must provide a corresponding setter/getter function. When processing an update, the synthesizer sets the inputs of the module via successive calls to the input setters. An input setter must not change the current output state of the module. When all inputs have been set, the synthesizer calls the update function of the module, which has no arguments. The update function must update the states of the output sockets by using the previously set input values.</p>
<p>Lets create a module:</p>"
				,(make-code-string "makedoc/snippets/module-blueprint.lisp")
				"<p>Now lets add our module to a rack (Racks are explained in the following chapter):</p>"
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
				"<p>A more comprehensive example can be found <a href=\"https://github.com/Frechmatz/cl-synthesizer/blob/master/patches/siren.lisp\">here</a>.</p>")
		       (heading (:name "Monitor" :toc t)
				"<p>Monitors are high-level Rack-Hooks. The purpose of a monitor
is to collect module states and pass them to a \"Monitor-Backend\". A backend may
for example generate a Wave file.</p> 
<p>Monitors provide a simple syntax for declaring the module sockets to be tracked (input, output and state) as well as any other settings supported by specific backends.</p>
<p>A backend is typically realized as a plain module, which declares
input sockets, output sockets, a shutdown function and so on. Backends are 
instantiated by a so called \"Monitor-Handler\".</p>
<p>A Monitor-Handler is a factory function whose purpose is to prepare the initialization 
parameters of a specific backend (e.g. a Wave-File-Writer module), to set up the 
mapping of the values collected by the monitor to input sockets of the backend 
and finally to instantiate it.</p>"
				,(make-example-header)
				,(make-code-string "src/monitor/monitor/example-1.lisp")))
	      (heading (:name "API" :toc t)
		       (heading (:toc t :name "Environment")
				(heading (:toc t :name "make-environment")
					 ,(make-function-string lib-index "cl-synthesizer" "make-environment")))
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
		       (heading
			(:toc t :name "Modules")
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
			(heading (:toc t :name "MIDI CC Interface")
				 ,(make-function-string lib-index "cl-synthesizer-modules-midi-cc-interface" "make-module")
				 ,(make-example-header)
				 ,(make-code-string "src/modules/midi-cc-interface/example-1.lisp"))
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
				(heading (:toc t :name "wave-handler")
					 ,(make-function-string lib-index "cl-synthesizer-monitor-wave-handler" "make-backend"))
				(heading (:toc t :name "csv-handler")
					 ,(make-function-string lib-index "cl-synthesizer-monitor-csv-handler" "make-backend")))
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
				,(make-function-string lib-index "cl-synthesizer-midi-event" "get-controller-value")
				,(make-function-string lib-index "cl-synthesizer-midi-event" "get-note-number")
				,(make-function-string lib-index "cl-synthesizer-midi-event" "get-velocity")
				(heading (:toc t :name "MIDI Utilities")
					 ,(make-function-string lib-index "cl-synthesizer-midi" "get-note-number-frequency")))
		       (heading (:toc t :name "Conditions"))
		       ,(make-condition-string lib-index "cl-synthesizer" "assembly-error"))
	      (heading (:name "Acknowledgements" :toc t)
		       ,(cl-html-readme:read-file "makedoc/acknowledge.html")))
    (semantic (:name "footer")
	      "<p><small>Generated " ,(now) "</small></p>")
    "</body></html>"))

(defun get-patches (lib-index examples-index)
  (declare (ignore lib-index examples-index))
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
	      "<p><small>Generated " ,(now) "</small></p>")
    "</body></html>"))


(defun make-readme ()
  ;; Generate patches
  (cl-synthesizer-patches-siren::run-example)
  (cl-synthesizer-patches-sine::run-example)
  ;; Generate html files
  (let ((lib-index (docparser:parse :cl-synthesizer))
	(examples-index (docparser:parse :cl-synthesizer-examples)))
    (docparser:dump lib-index)
    (let ((cl-html-readme:*home-directory* "/Users/olli/src/lisp/cl-synthesizer/")
	  (cl-html-readme:*tab-width* 8))
      (with-open-file (fh (cl-html-readme:make-path "docs/index.html")
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :external-format :utf-8)
	(cl-html-readme:doc-to-html fh (get-readme lib-index)))
      (with-open-file (fh (cl-html-readme:make-path "docs/patches.html")
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :external-format :utf-8)
	(cl-html-readme:doc-to-html fh (get-patches lib-index examples-index)))))
    "DONE")

;;(make-readme)

