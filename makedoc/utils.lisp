(in-package :cl-synthesizer-makedoc)

;;
;; Todos
;; - define with-heading macros to reduce bloat
;;

(defun make-function-string (f)
  (concatenate
   'string
   "<p>"
   (cl-readme:sbcl-make-function-decl f)
   "</p><p>"
   (documentation f 'function)
   "</p>"))

(defun make-condition-string (c)
  (concatenate
   'string
   "<b>" (string-downcase (symbol-name c)) "</b>"
   "<p>"
   (documentation c 'type)
   "</p>"))

(defun make-package-string (p)
  (concatenate
   'string
   "<p>" (documentation (find-package p) t)
   "</p>"))

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
