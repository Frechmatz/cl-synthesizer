;;
;; Rack compiler
;;

(in-package :cl-synthesizer-rack-compiler)


(defun iterate-module-input-patches (rack module callback)
  "cb: lambda (input-socket output-module output-module-socket)"
  (let ((module-name (cl-synthesizer:get-module-name rack module))
	(patches (cl-synthesizer:get-patches rack)))
    (cl-synthesizer-property-list-iterator:do-property-list-keys
	(funcall (getf module :inputs))
	cur-input-socket
      (let ((patch
	      (find-if
	       (lambda (p)
		 (and
		  (string= (getf p :input-name) module-name)
		  (eq (getf p :input-socket) cur-input-socket)))
	       patches)))
	(if patch
	    (let ((output-module
		    (cl-synthesizer:get-module rack (getf patch :output-name)))
		  (input-socket cur-input-socket)
		  (output-module-socket (getf patch :output-socket)))
	      (funcall callback input-socket output-module output-module-socket)))))))

(defun get-module-trace (rack)
  "Get list of modules in execution order"
  (let ((module-trace nil)
	(visited-modules nil))
    (labels ((traverse-module (module)
	       (if (not (find module visited-modules :test #'eq))
		   (progn
		     (push module visited-modules)
		     (iterate-module-input-patches
		      rack
		      module
		      (lambda (input-socket output-module output-module-socket)
			(declare (ignore input-socket output-module-socket))
			(traverse-module output-module)))
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
    (iterate-module-input-patches
     rack
     module
     (lambda (cur-input-socket output-module output-socket)
       (let ((input-setter (getf (getf inputs cur-input-socket) :set)))
	 (let ((output-getter (make-get-output-lambda output-module output-socket)))
	   (push (lambda()
		   (funcall input-setter (funcall output-getter)))
		 input-setters)))))
    ;; The compiled update function
    (lambda ()
      ;; Set inputs
      (dolist (fn input-setters)
	(funcall fn))
      ;; Update module
      (funcall module-update-fn))))

(defun compile-rack (rack)
  "Compile a rack."
  (let ((module-updates
	  (mapcar
	   (lambda (module) (compile-module rack module))
	   (get-module-trace rack)))
	(rack-updated-hooks nil)
	(rack-updating-hooks nil))
    (dolist (hook (funcall (getf rack :hooks)))
      (let ((updated (getf hook :updated))
	    (updating (getf hook :updating)))
	(if updating (push updating rack-updating-hooks))
	(if updated (push updated rack-updated-hooks))))
    (lambda ()
      ;; Call hooks
      (dolist (fn rack-updating-hooks)
	(funcall fn))
      ;; Update modules
      (dolist (fn module-updates)
	(funcall fn))
      ;; Call hooks
      (dolist (fn rack-updated-hooks)
	(funcall fn)))))

