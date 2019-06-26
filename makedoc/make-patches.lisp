(in-package :cl-synthesizer-makepatches)


(defun audio (filename)
  (concatenate
   'string
   "<audio controls=\"controls\" preload=\"none\">"
   "Your browser does not support the <code>audio</code> element."
   "<source src=\""
   (format nil "~a?cb=~a" filename (get-universal-time))
   "\" type=\"audio/wav\">"
   "</audio>"))

(defun write-html ()
  (let ((cl-readme:*home-directory* "/Users/olli/src/lisp/cl-synthesizer/")
	(cl-readme:*tab-width* 8))
    (let ((docstr (concatenate
		   'string
		   "<html><body>"
		   "<h1>Example patches for cl-synthesizer</h1>"
		   "Work in progress."
		   "<h2>Siren</h2>"
		   "<p>"
		   (audio "siren.wav")
		   (cl-readme:example-code "patches/siren.lisp" :omit-header t)
		   "</p>"
		   "<hr/><p><small>Generated " (cl-readme:current-date) "</small></p>"
		   "</body></html>")))
      (with-open-file (fh (cl-readme:make-path "docs/patches.html")
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :external-format :utf-8)
	(format fh "~a" docstr)))))

;;(write-html)

