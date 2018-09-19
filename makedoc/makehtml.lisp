(defpackage :cl-synthesizer-makedoc (:use :cl))
(in-package :cl-synthesizer-makedoc)

;; uses sbcl extensions
(defun make-function-declaration-string (f)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((f-name (symbol-name f))
	(f-lambda-list-str
	 (mapcar
	  ;; if item is not a symbol then omit it
	  ;; this is the case for default values of key arguments
	  (lambda (item) (if (symbolp item) (symbol-name item) ""))
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

(defun make-function-string (f)
  (concatenate
   'string
   "<p>"
   (make-function-declaration-string f)
   "</p><p>"
   (documentation f 'function)
   "</p>"))

(defun current-date ()
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (get-decoded-time)
    (declare (ignore dow dst-p tz))
    ;; 2018-09-19 21:28:16
    (format nil "~4,'0d-~2,'0d-~2,'0d  ~2,0d:~2,0d:~2,0d" yr mon day hr min sec)))
  
(defun write-html ()
    (let ((docstr (concatenate
		   'string
		   "<html><body>"
		   "<h1>cl-synthesizer</h1>"
		   "An experimental modular audio synthesizer implemented in Common Lisp."
		   "<p>Work in progress...</p>"
		   "<h2>Rack</h2>"
		   (make-function-string 'cl-synthesizer:make-rack)
		   (make-function-string 'cl-synthesizer:add-module)
		   (make-function-string 'cl-synthesizer:add-patch)
		   (make-function-string 'cl-synthesizer:update)
		   (make-function-string 'cl-synthesizer:play-rack)
		   (make-function-string 'cl-synthesizer:make-environment)
		   (make-function-string 'cl-synthesizer:get-environment)
		   (make-function-string 'cl-synthesizer:get-module)
		   (make-function-string 'cl-synthesizer:get-patch)
		   (make-function-string 'cl-synthesizer:get-line-out-adapter)
		   (make-function-string 'cl-synthesizer:get-midi-in-adapter)
		   (make-function-string 'cl-synthesizer:add-hook)
		   "<h2>Modules</h2>"
		   (make-function-string 'cl-synthesizer-modules-vco:vco-linear)
		   (make-function-string 'cl-synthesizer-modules-vco:vco-exponential)
		   (make-function-string 'cl-synthesizer-modules-vca:vca)
		   (make-function-string 'cl-synthesizer-modules-multiple:multiple)
		   (make-function-string 'cl-synthesizer-modules-midi-sequencer:midi-sequencer)
		   (make-function-string 'cl-synthesizer-modules-midi-interface:midi-interface)
		   (make-function-string 'cl-synthesizer-modules-envelope:envelope)
		   (make-function-string 'cl-synthesizer-modules-fixed-output:fixed-output)
		   "<h2>Monitor</h2>"
		   (make-function-string 'cl-synthesizer-monitor:add-monitor)
		   (documentation 'cl-synthesizer-monitor:add-monitor 'function)
		   "<h2>Devices</h2>"
		   (make-function-string 'cl-synthesizer-device-speaker:speaker-cl-out123)
		   (make-function-string 'cl-synthesizer-device-midi:midi-device)
		   "<p><small>Readme generated " (current-date) "</small></p>"
		   "</body></html>"
		   )))
      (with-open-file (fh "/Users/olli/src/lisp/cl-synthesizer/makedoc/doc.html"
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
	(format fh "~a" docstr))))

;;(write-html)

	
      
			       
