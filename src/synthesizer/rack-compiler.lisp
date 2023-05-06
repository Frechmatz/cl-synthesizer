;;
;; Rack compiler
;;

(in-package :cl-synthesizer-rack-compiler)

(defmacro do-property-list (plist property-key property-value &body body)
  (let ((read-key (gensym)) (cur-key (gensym)) (item (gensym)))
    `(let ((,read-key t) (,cur-key nil))
       (dolist (,item ,plist)
	 (if ,read-key
	     (progn
	       (setf ,read-key nil)
	       (setf ,cur-key ,item))
	     (progn
	       (setf ,read-key t)
	       (let ((,property-key ,cur-key) (,property-value ,item))
		 ,@body)))))))

(defmacro do-module-input-patches (rack module input-socket output-module output-module-socket
				   &body body)
  (let ((patch (gensym))
	(module-name (gensym))
	(cur-input-socket (gensym))
	(cur-input-socket-value (gensym))
	(patches (gensym)))
    `(let ((,module-name (cl-synthesizer:get-module-name ,rack ,module))
	   (,patches (cl-synthesizer:get-patches ,rack)))
       (do-property-list
	   (funcall (getf ,module :inputs))
	   ,cur-input-socket
	   ,cur-input-socket-value
	 (declare (ignore ,cur-input-socket-value))
	 (let ((,patch
		 (find-if
		  (lambda (p)
		    (and
		     (string= (getf p :input-name) ,module-name)
		     (eq (getf p :input-socket) ,cur-input-socket)))
		  ,patches)))
	   (if ,patch
	       (let ((,output-module
		       (cl-synthesizer:get-module ,rack (getf ,patch :output-name)))
		     (,input-socket ,cur-input-socket)
		     (,output-module-socket
		       (getf ,patch :output-socket)))
		 ,@body)))))))

(defun get-module-trace (rack)
  "Get list of modules in execution order"
  (let ((module-trace nil)
	(visited-modules nil))
    (labels ((traverse-module (module)
	       (if (not (find module visited-modules :test #'eq))
		   (progn
		     (push module visited-modules)
		     (do-module-input-patches
			 rack
			 module
			 input-socket
			 output-module
			 output-module-socket
		       (declare (ignore input-socket output-module-socket))
		       (traverse-module output-module))
		     (push module module-trace)))))
      (dolist (module (cl-synthesizer:get-modules rack))
	(traverse-module (getf module :module)))
      (nreverse module-trace))))

(defun make-get-output-lambda (module output-socket)
  (getf (getf (funcall (getf module :outputs)) output-socket) :get))

(defun compile-module (rack module)
  (let ((input-setters nil)
	(inputs (funcall (getf module :inputs)))
	(module-update-fn (getf module :update)))
    ;; Push setters for all inputs
    (do-module-input-patches
	rack
	module
	cur-input-socket
	output-module
	output-socket
      (let ((input-setter (getf (getf inputs cur-input-socket) :set)))
	(let ((output-getter (make-get-output-lambda output-module output-socket)))
	  (push (lambda()
		  (funcall input-setter (funcall output-getter)))
		input-setters))))
    ;; The compiled update function
    (lambda ()
      ;; Set inputs
      (dolist (fn input-setters)
	(funcall fn))
      ;; Update module
      (funcall module-update-fn))))

(defun compile-rack (rack)
  "Compile a rack."
  (let ((lambdas (mapcar (lambda (module) (compile-module rack module)) (get-module-trace rack))))
    (lambda ()
      ;; Update modules
      (dolist (fn lambdas)
	(funcall fn))
      ;; Call hooks
      (dolist (h (cl-synthesizer:get-hooks rack))
	(cl-synthesizer:update h)))))

