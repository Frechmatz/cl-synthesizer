(in-package :cl-synthesizer-makedoc)

(defun get-node (index package-name symbol-name)
  (aref (docparser:query
	 index
	 :package-name (string-upcase package-name)
	 :symbol-name (string-upcase symbol-name))
	0))

(defun make-function-string (index package-name symbol-name)
  (let* ((node (get-node index package-name symbol-name))
	 (lambda-list (docparser:operator-lambda-list node)))
    (concatenate
     'string
     "<b>" (string-downcase symbol-name) "</b>&nbsp;"
     (string-downcase (format nil "~a" (if lambda-list lambda-list "()")))
     "<p>" (docparser:node-docstring node) "</p>")))


(defun make-condition-string (index package-name symbol-name)
  (let* ((node (get-node index package-name symbol-name)))
 (concatenate
  'string
  "<b>" (string-downcase symbol-name) "</b>"
  "<p>"  (docparser:node-docstring node) "</p>")))

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

(defparameter *show-code* "Show patch")
(defparameter *hide-code* "Hide patch")

#|
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
|#

(defun make-patch (&key package code wave-file (show-code nil))
  (declare (ignore package code wave-file show-code))
  "<p>XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX</p>"
  ;; (let ((link-id (gensym)) (source-code-id (gensym)))
  ;;   (list 'heading (list :name (make-package-string package) :toc t)
  ;; 	  (make-audio-element wave-file)
  ;; 	  (format nil "<p><a href=\"#\" id=\"~a\"" link-id)
  ;; 	  (format nil "onclick=\"return toggleDisplay('~a', '~a', '~a', '~a')\">~a</a></p>"
  ;; 		  *show-code* *hide-code* link-id source-code-id (if show-code *hide-code* *show-code*))
  ;; 	  ;; TODO Pass id to read-code
  ;; 	  (format nil
  ;; 		  "<div style=\"display: ~a\" id='~a'>~a</div>"
  ;; 		  (if show-code "block" "none")
  ;; 		  source-code-id
  ;; 		  (cl-readme:read-code code)))))
  )

(defun now ()
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (get-decoded-time)
    (declare (ignore dow dst-p tz))
    ;; 2018-09-19 21:28:16
    (let ((str (format nil "~4,'0d-~2,'0d-~2,'0d  ~2,'0d:~2,'0d:~2,'0d" yr mon day hr min sec)))
      str)))
