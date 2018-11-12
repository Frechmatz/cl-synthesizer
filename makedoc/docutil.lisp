(in-package :cl-synthesizer-doc-util)

(defparameter *CL-SYNTHESIZER-HOME* "/Users/olli/src/lisp/cl-synthesizer/")

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

(defun make-path (path)
  (concatenate 'string *CL-SYNTHESIZER-HOME* path))


(defun read-text-file (path)
  (let ((output (make-array '(0) :element-type 'base-char
			    :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s output)
      (with-open-file (fh (make-path path) :direction :input :external-format :utf-8)
	(loop 
	   (let ((str (read-line fh nil)))
	     (if (not str)
		 (return)
		 (format s "~a~%" str))))))
    (string-trim '(#\Space #\Tab #\Newline) output)))

(defun example-code (path &key (example-number nil))
  (concatenate 'string
	       "<p><b>Example"
	       (if example-number (format nil " ~a" example-number) "")
	       ":</b></p>"
	       "<p><pre><code>"
	       (read-text-file path)
	       "</code></pre></p>"))

