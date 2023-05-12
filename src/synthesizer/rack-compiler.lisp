;;
;; Rack compiler
;;

(in-package :cl-synthesizer-rack-compiler)

;;
;; Iterator macros
;; Todos
;; - Support of declare expressions analog to dolist. Does not work right now
;;   due to required wrapping of body into a progn form
;; - after that removal of do-module-input-patches-output-modules
;;

(defmacro do-module-input-patches (rack module input-socket output-module
				   output-module-socket &body body)
  (let ((patch (gensym))
	(module-name (gensym))
	(cur-input-socket (gensym))
	(patches (gensym)))
    `(let ((,module-name (cl-synthesizer:get-module-name ,rack ,module))
	   (,patches (cl-synthesizer:get-patches ,rack)))
       (cl-synthesizer-property-list-iterator:do-property-list-keys
	   (funcall (getf ,module :inputs))
	   ,cur-input-socket
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
		 (progn ,@body))))))))

(defmacro do-module-input-patches-output-modules (rack module
						  output-module &body body)
  (let ((patch (gensym))
	(module-name (gensym))
	(cur-input-socket (gensym))
	(patches (gensym)))
    `(let ((,module-name (cl-synthesizer:get-module-name ,rack ,module))
	   (,patches (cl-synthesizer:get-patches ,rack)))
       (cl-synthesizer-property-list-iterator:do-property-list-keys
	   (funcall (getf ,module :inputs))
	   ,cur-input-socket
	 (let ((,patch
		 (find-if
		  (lambda (p)
		    (and
		     (string= (getf p :input-name) ,module-name)
		     (eq (getf p :input-socket) ,cur-input-socket)))
		  ,patches)))
	   (if ,patch
	       (let ((,output-module
		       (cl-synthesizer:get-module ,rack (getf ,patch :output-name))))
		 (progn ,@body))))))))

;;
;;
;;

(defun get-module-trace (rack)
  "Get list of modules in execution order"
  (let ((module-trace nil)
	(visited-modules nil))
    (labels ((traverse-module (module)
	       (if (not (find module visited-modules :test #'eq))
		   (progn
		     (push module visited-modules)
		     (do-module-input-patches-output-modules
			 rack
			 module
			 output-module
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

