(in-package :cl-synthesizer-makedoc)

(defun make-id (name)
  ;; Table of content in Github flavored markdown
  ;; https://gist.github.com/asabaylus/3071099
  ;; tolowercase
  ;; spaces durch - ersetzen
  (format nil "Generated-Id-~a" name))

(defun generate-html (output-stream my-list)
  ""
    (labels ((section-p (l)
	       (and (symbolp (first l)) (string= (symbol-name (first l)) "SECTION")))
	     (toc-p (i)
	       (and (symbolp i) (string= (symbol-name i) "TOC")))
	     (section-settings (section)
	       (second section))
	     (section-settings-name (section)
	       (getf (section-settings section) :name))
	     (section-settings-id (section)
	       (getf (section-settings section) :id))
	     (section-settings-toc (section)
	       (getf (section-settings section) :toc))
	     (toc-section-p (l)
	       (and (section-p l) (getf (section-settings l) :toc)))
	     (toc-section-section-p (l)
	       (and (section-p l) (eq :section (getf (section-settings l) :toc))))
	     (toc-section-item-p (l)
	       (and (section-p l) (eq :item (getf (section-settings l) :toc))))

	     )
      
      
      (labels ((rewrite (l)
		 ;;(declare (optimize (debug 3) (speed 0) (space 0)))
		 (cond
		   ((not (listp l))
		    l)
		   ((functionp (first l))
		    ;; Evaluate and rewrite result
		    (rewrite (apply (first l) (rest l))))
		   ((toc-section-p l)
		    (let ((section (list)))
		      ;; Rewrite section element
		      (push 'SECTION section)
		      (push
		       (list
			:id (make-id (section-settings-name l))
			:toc (section-settings-toc l)
			:name (section-settings-name l))
		       section)
		      (dolist (item (rest (rest l)))
			(push (rewrite item) section))
		      (reverse section)
		      ))
		   (t
		    (let ((cloned-list (list)))
		      (dolist (item l)
			(push (rewrite item) cloned-list))
		      (reverse cloned-list)))))

	       (validate (l)
		 ;; TODO Validate section-settings :toc Must be nil or :item or :section
		 (cond
		   ((not (listp l))
		    nil)
		   ((section-p l)
		    (if (not (section-settings-name l))
			(error (format nil "Section must have a :name property ~a" l)))
		    (dolist (item (rest (rest l)))
		      (validate item)))
		   (t
		    (dolist (item l)
		      (validate item)))))

	       (generate-html-toc (l)
		 (labels ((generate-html-toc-impl (sub-list)
			    (cond
			      ((not (listp sub-list))
			       nil)
			      ((toc-section-section-p sub-list)
			       ;; <li>...</li><ul>...</ul>
			       (format
				output-stream "<li><a href=\"#~a\">~a</a><ul>"
				(section-settings-id sub-list)
				(section-settings-name sub-list))
			       (dolist (item (rest (rest sub-list)))
				 (generate-html-toc-impl item))
			       (format output-stream "</ul></li>"))
			      ((toc-section-item-p sub-list)
			       ;; <li>...</li>
			       (format
				output-stream "<li><a href=\"#~a\">~a</a></li>"
				(section-settings-id sub-list)
				(section-settings-name sub-list))
			       (dolist (item (rest (rest sub-list)))
				 (generate-html-toc-impl item))
			       )
			      (t
			       (dolist (item sub-list)
				 (generate-html-toc-impl item))))))
		   (format output-stream "<ul>")
		   (generate-html-toc-impl l)
		   (format output-stream "</ul>")
		   ))

	       
	       (generate-html (l)
		 (labels ((generate-html-impl (heading-level sub-list)
			    ;; TODO Render TOC
			    (cond
			      ((toc-p sub-list)
			       (generate-html-toc l))
			      ((not (listp sub-list))
			       (format output-stream "~a" sub-list))
			      ((section-p sub-list)
			       (format
				output-stream "<h~a~a>~a</h~a>"
				(+ 1 heading-level)
				(if (section-settings-id sub-list)
				    (format nil " id=\"~a\"" (section-settings-id sub-list))
				    "")
				(section-settings-name sub-list)
				(+ 1 heading-level))
			       (dolist (item (rest (rest sub-list)))
				 (generate-html-impl (+ 1 heading-level) item)))
			      (t
			       (dolist (item sub-list)
				 (generate-html-impl heading-level item))))))
		   (generate-html-impl 0 l)))
	       
	       )
	
	(let ((rewritten (rewrite my-list)))
	  (validate rewritten)
	  (generate-html rewritten)
	  nil
	)))

      )

