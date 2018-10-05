(defpackage :cl-synthesizer-makedoc (:use :cl))
(in-package :cl-synthesizer-makedoc)

(defun lambda-list-arg-to-string (arg)
  (cond
    ((keywordp arg)
     (format nil ":~a" arg))
    ((symbolp arg)
     (symbol-name arg))
    ((stringp arg)
     (format nil "\"~a\"" arg))
    ((listp arg)
     (concatenate 'string
		  "("
		  (reduce
		   (lambda(buffer item)
		     (concatenate 'string buffer
				  (if (> (length buffer) 0) " " "")
				  (lambda-list-arg-to-string item)))
		   arg
		   :initial-value "")
		  ")"))
    (t (format nil "~a" arg))))

;; uses sbcl extensions
(defun make-function-declaration-string (f)
  (let ((f-name (symbol-name f))
	(f-lambda-list-str
	 (mapcar
	  (lambda (item) (lambda-list-arg-to-string item))
	  (sb-introspect:function-lambda-list f))))
    (let ((ll (reduce
	       (lambda(buffer item) (concatenate 'string buffer item " "))
	       f-lambda-list-str
	       :initial-value "")))
      (concatenate 'string
		   "<b>"
		   (string-downcase (package-name (symbol-package f)))
		   ":"
		   (string-downcase f-name)
		   "</b>"
		   " "
		   (string-downcase ll)))))

(defun make-function-string (f &key (append-separator t))
  (concatenate
   'string
   "<p>"
   (make-function-declaration-string f)
   "</p><p>"
   (documentation f 'function)
   "</p>"
   (if append-separator "<hr/>" "")))

(defun make-condition-string (f &key (append-separator t))
  (concatenate
   'string
   "<b>" (string-downcase (symbol-name f)) "</b>"
   "<p>"
   (documentation f 'type)
   "</p>"
   (if append-separator "<hr/>" "")))

(defun current-date ()
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (get-decoded-time)
    (declare (ignore dow dst-p tz))
    ;; 2018-09-19 21:28:16
    (let ((str (format nil "~4,'0d-~2,'0d-~2,'0d  ~2,'0d:~2,'0d:~2,'0d" yr mon day hr min sec)))
      str)))

(defun read-text-file (path)
  (with-output-to-string (s)
    (with-open-file (fh path :direction :input :external-format :utf-8)
      (loop 
	 (let ((str (read-line fh nil)))
	   (if (not str)
	       (return)
	       (format s "~a~%" str)))))))

(defun example-code (path &key (example-number nil))
  (concatenate 'string
	       "<p><b>Example"
	       (if example-number (format nil " ~a" example-number) "")
	       ":</b></p>"
	       "<p><pre><code>"
	       (read-text-file path)
	       "</code></pre></p>"))

(defun write-html ()
    (let ((docstr (concatenate
		   'string
		   "<html><body>"
		   "<h1>cl-synthesizer</h1>"
		   "An experimental modular audio synthesizer implemented in Common Lisp. Work in progress."
		   "<p>" (documentation 'cl-synthesizer::rack 'type) "</p>"
		   (example-code "/Users/olli/src/lisp/cl-synthesizer/src/synthesizer/rack/example-1.lisp")
		   "<h2>Installation</h2>"
		   (read-text-file "/Users/olli/src/lisp/cl-synthesizer/makedoc/installation.html")
		   "<h2>API Reference</h2>"
		   ;; Table of content in Github flavored markdown
		   ;; https://gist.github.com/asabaylus/3071099
		   "<ul>"
		   "<li><a href=\"#environment\">Environment</a></li>"
		   "<li><a href=\"#rack\">Rack</a></li>"
		   "<li><a href=\"#modules\">Modules</a></li>"
		   "<li><a href=\"#midi\">MIDI</a></li>"
		   "<li><a href=\"#monitor\">Monitor</a></li>"
		   "<li><a href=\"#device\">Device</a></li>"
		   "<li><a href=\"#conditions\">Conditions</a></li>"
		   "</ul>"
		   "<h3>Environment</h3>"
		   (make-function-string 'cl-synthesizer:make-environment :append-separator nil)
		   "<h3>Rack</h3>"
		   (make-function-string 'cl-synthesizer:make-rack)
		   (make-function-string 'cl-synthesizer:add-module)
		   (make-function-string 'cl-synthesizer:add-patch)
		   (make-function-string 'cl-synthesizer:update)
		   (make-function-string 'cl-synthesizer:play-rack)
		   (make-function-string 'cl-synthesizer:get-environment)
		   (make-function-string 'cl-synthesizer:get-module)
		   (make-function-string 'cl-synthesizer:get-patch)
		   (make-function-string 'cl-synthesizer:add-hook)
		   (make-function-string 'cl-synthesizer:shutdown :append-separator nil)
		   "<h3>Modules</h3>"
		   (make-function-string 'cl-synthesizer-modules-vco:vco-base)
		   (make-function-string 'cl-synthesizer-modules-vco:vco-linear :append-separator nil)
		   (example-code "/Users/olli/src/lisp/cl-synthesizer/src/modules/vco/example-1.lisp")
		   (make-function-string 'cl-synthesizer-modules-vco:vco-exponential)
		   (make-function-string 'cl-synthesizer-modules-vca:vca :append-separator nil)
		   (example-code "/Users/olli/src/lisp/cl-synthesizer/src/modules/vca/example-1.lisp")
		   (make-function-string 'cl-synthesizer-modules-multiple:multiple)
		   (make-function-string 'cl-synthesizer-modules-midi-sequencer:midi-sequencer :append-separator nil)
		   (example-code "/Users/olli/src/lisp/cl-synthesizer/src/modules/midi-sequencer/example-1.lisp")
		   (make-function-string 'cl-synthesizer-modules-midi-interface:midi-interface :append-separator nil)
		   (example-code "/Users/olli/src/lisp/cl-synthesizer/src/modules/midi-interface/example-1.lisp")
		   (make-function-string 'cl-synthesizer-modules-envelope:envelope :append-separator nil)
		   (example-code "/Users/olli/src/lisp/cl-synthesizer/src/modules/envelope/example-1.lisp")
		   (make-function-string 'cl-synthesizer-modules-fixed-output:fixed-output :append-separator nil)
		   "<h3>MIDI</h3>"
		   "<h4>MIDI Event</h4>"
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
		   "<h4>MIDI Utilities</h4>"
		   (make-function-string 'cl-synthesizer-midi:get-note-number-frequency)
		   (make-function-string 'cl-synthesizer-midi:relative-cc-handler :append-separator nil)
		   (example-code "/Users/olli/src/lisp/cl-synthesizer/src/midi/cc-handler/example-1.lisp" :example-number 1)
		   (example-code "/Users/olli/src/lisp/cl-synthesizer/src/midi/cc-handler/example-2.lisp" :example-number 2)
		   "<h3>Monitor</h3>"
		   (make-function-string 'cl-synthesizer-monitor:add-monitor)
		   (make-function-string 'cl-synthesizer-monitor-wave-handler:wave-file-handler :append-separator nil)
		   "<h3>Device</h3>"
		   (make-function-string 'cl-synthesizer:attach-audio-device)
		   (make-function-string 'cl-synthesizer:attach-midi-in-device)
		   (make-function-string 'cl-synthesizer:make-device)
		   (make-function-string 'cl-synthesizer-device-speaker:speaker-cl-out123)
		   (make-function-string 'cl-synthesizer-device-midi:midi-device :append-separator nil)
		   "<h3>Conditions</h3>"
		   (make-condition-string 'cl-synthesizer:assembly-error :append-separator nil)
		   ;;(documentation 'cl-synthesizer:assembly-error 'type)
		   "<hr/><p><small>Generated " (current-date) "</small></p>"
		   "</body></html>"
		   )))
      (with-open-file (fh "/Users/olli/src/lisp/cl-synthesizer/makedoc/doc.html"
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :external-format :utf-8)
	(format fh "~a" docstr))))

;;(write-html)

