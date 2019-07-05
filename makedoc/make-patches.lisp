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
		   "<html>"
		   "<head>"
		   "<link rel=\"stylesheet\" href=\"//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic\">"
		   "<link rel=\"stylesheet\" href=\"//cdn.rawgit.com/necolas/normalize.css/master/normalize.css\">"
		   "<link rel=\"stylesheet\" href=\"//cdn.rawgit.com/milligram/milligram/master/dist/milligram.min.css\">"
		   "</head>"
		   "<body>"
		   "<section class=\"container\">"
		   "<h1>Example patches for cl-synthesizer</h1>"
		   "<p>Work in progress...</p>"
		   "<p>Back to the <a href=\"https://frechmatz.github.io/cl-synthesizer/\">project site.</a></p>"
		   "</section>"
		   "<section class=\"container\">"
		   "<h2>Siren</h2>"
		   "<p>"
		   (audio "siren.wav")
		   (cl-readme:example-code "patches/siren.lisp" :omit-header t)
		   "</p>"
		   "</section>"
		   "<section class=\"container\">"
		   "<hr/><p><small>Generated " (cl-readme:current-date) "</small></p>"
		   "</section>"
		   "</body></html>")))
      (with-open-file (fh (cl-readme:make-path "docs/patches.html")
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :external-format :utf-8)
	(format fh "~a" docstr)))))

;;(write-html)

