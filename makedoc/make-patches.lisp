(in-package :cl-synthesizer-makepatches)

(defun make-audio-element (filename)
  (concatenate
   'string
   "<audio controls=\"controls\" preload=\"none\">" ;; loop=\"false\">"
   "Your browser does not support the <code>audio</code> element."
   "<source src=\""
   ;; Add query parameter with timestamp as a cache buster
   (format nil "~a?cb=~a" filename (get-universal-time))
   "\" type=\"audio/wav\">"
   "</audio>"))

(defun make-package-string (p)
  (concatenate
   'string
   "<p><b>" (documentation (find-package p) t)
   "</b></p>"))

(defparameter *show-code* "Show patch")
(defparameter *hide-code* "Hide patch")

(defun make-patch (&key package code wave-file (show-code nil))
  (let ((link-id (gensym)) (source-code-id (gensym)))
    (list 'heading (list :name (make-package-string package) :toc t)
	  (make-audio-element wave-file)
	  (format nil "<p><a href=\"#\" id=\"~a\"" link-id)
	  (format nil "onclick=\"return toggleDisplay('~a', '~a', '~a', '~a')\">~a</a></p>"
		  *show-code* *hide-code* link-id source-code-id (if show-code *hide-code* *show-code*))
	  ;; TODO Pass id to read-code
	  (format nil
		  "<div style=\"display: ~a\" id='~a'>~a</div>"
		  (if show-code "block" "none")
		  source-code-id
		  (cl-readme:read-code code)))))


(defun get-doc ()
  (let ((tree
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
		     ,(make-patch :package 'cl-synthesizer-patches-siren
				  :code "patches/siren.lisp"
				  :wave-file "siren.wav"))
	   (semantic (:name "footer")
		     "<p><small>Generated " ,(cl-readme:current-date) "</small></p>")
	   "</body></html>")))
    tree))

(defclass cl-synthesizer-readme-writer (cl-readme:html-writer) ())

(defmethod cl-readme:open-semantic ((writer cl-synthesizer-readme-writer) semantic-element-settings)
  (format nil "<~a class=\"container\">" (getf semantic-element-settings :name)))

(defun make-readme ()
  ;; Generate patches
  (cl-synthesizer-patches-siren::run-example)
  ;; Generate doc
  (let ((cl-readme:*home-directory* "/Users/olli/src/lisp/cl-synthesizer/")
	(cl-readme:*tab-width* 8))
    (with-open-file (fh (cl-readme:make-path "docs/patches.html")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf-8)
      (let ((w (make-instance 'cl-synthesizer-readme-writer)))
	(cl-readme:doc-to-html w fh (get-doc))))
  "DONE"))

;;(make-readme)

