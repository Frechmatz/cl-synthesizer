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

(defun write-html ()
    (let ((docstr (concatenate
		   'string
		   "<html><body>"
		   "<h1>cl-synthesizer</h1>"
		   "An experimental modular audio synthesizer implemented in Common Lisp. Work in progress."
		   "<p>" (documentation 'cl-synthesizer::rack 'type) "</p>"
		   "<p><b>Example:</b></p>"
		   "<p><pre><code>"
		   (read-text-file "/Users/olli/src/lisp/cl-synthesizer/src/synthesizer/rack/example-1.lisp")
		   "</code></pre></p>"
		   "<h2>Installation</h2>"
		   "<p>TODO</p>"
		   "<h2>API Reference</h2>"
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
		   (make-function-string 'cl-synthesizer:get-line-out-adapter)
		   (make-function-string 'cl-synthesizer:get-midi-in-adapter)
		   (make-function-string 'cl-synthesizer:add-hook :append-separator nil)
		   "<h3>Modules</h3>"
		   (make-function-string 'cl-synthesizer-modules-vco:vco-base)
		   (make-function-string 'cl-synthesizer-modules-vco:vco-linear)
		   (make-function-string 'cl-synthesizer-modules-vco:vco-exponential)
		   (make-function-string 'cl-synthesizer-modules-vca:vca)
		   (make-function-string 'cl-synthesizer-modules-multiple:multiple)
		   (make-function-string 'cl-synthesizer-modules-midi-sequencer:midi-sequencer)
		   (make-function-string 'cl-synthesizer-modules-midi-interface:midi-interface)
		   (make-function-string 'cl-synthesizer-modules-envelope:envelope)
		   (make-function-string 'cl-synthesizer-modules-fixed-output:fixed-output :append-separator nil)
		   "<h3>Monitor</h3>"
		   (make-function-string 'cl-synthesizer-monitor:add-monitor)
		   (make-function-string 'cl-synthesizer-monitor-wave-handler:wave-file-handler :append-separator nil)
		   "<h3>Device</h3>"
		   (make-function-string 'cl-synthesizer:make-device)
		   (make-function-string 'cl-synthesizer-device-speaker:speaker-cl-out123)
		   (make-function-string 'cl-synthesizer-device-midi:midi-device :append-separator nil)
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

