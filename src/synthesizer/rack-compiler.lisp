;;
;; Rack compiler
;;

(in-package :cl-synthesizer-rack-compiler)

(defun get-module-name (rack module)
  (let ((match
	    (find-if
	     (lambda (cur-module) (eq module (getf cur-module :module)))
	     (cl-synthesizer:get-modules rack))))
    (if match (getf match :name) nil)))

(defun get-module-by-name (rack name)
  (let ((module
	 (find-if
	  (lambda (m) (string= name (getf m :name)))
	  (cl-synthesizer:get-modules rack))))
    (if module (getf module :module) nil)))

(defun get-input-sockets (module)
  "TODO Inefficient implementation, but for now live with it"
  (let ((sockets nil))
    (cl-synthesizer-macro-util:with-property-list (cl-synthesizer:get-inputs module) socket fn
      (declare (ignore fn))
      (push socket sockets))
    sockets))

(defun get-module-input-patches (rack module)
  "Returns a sparse list of (input-socket output-module output-socket)"
  (let ((result nil) (name (get-module-name rack module)))
    (dolist (input-socket (get-input-sockets module))
      (let ((patch
	     (find-if
	      (lambda (p)
		(and
		 (string= (cl-synthesizer:get-patch-input-name p) name)
		 (eq (cl-synthesizer:get-patch-input-socket p) input-socket)))
	      (cl-synthesizer:get-patches rack))))
	(if patch
	    (push (list
		   input-socket
		   (get-module-by-name rack (cl-synthesizer:get-patch-output-name patch))
		   (cl-synthesizer:get-patch-output-socket patch)) result)
	    (push (list input-socket nil nil) result))))
    result))

(defun get-module-trace (rack)
  "Get list of modules in execution order"
  (let ((module-trace nil)
	(visited-modules nil))
    ;; Mark INPUT bridge module as visited
    (push (get-module-by-name rack "INPUT") visited-modules)
    (labels ((traverse-module (module)
	       (if (not (find module visited-modules :test #'eq))
		   (progn
		     (push module visited-modules)
		       (dolist (binding (get-module-input-patches rack module))
			 (let ((output-module (second binding)))
			   (if output-module
			       (traverse-module output-module))))
		       (push module module-trace)))))
      (dolist (module (cl-synthesizer:get-modules rack))
	(traverse-module (getf module :module)))
      (nreverse module-trace))))

(defun make-get-output-lambda (module output-socket)
  (let ((l (getf (cl-synthesizer:get-outputs module) output-socket)))
    (lambda() (funcall l))))

(defun compile-module (rack module)
  (let ((input-setters nil)
	(inputs (cl-synthesizer:get-inputs module))
	(module-update-fn (cl-synthesizer:get-update-fn module)))
    ;; Push setters for all inputs
    (dolist (binding (get-module-input-patches rack module))
      (let ((cur-input-socket (first binding))
	    (output-module (second binding))
	    (output-socket (third binding)))
	(let ((input-setter (getf inputs cur-input-socket)))
	  (if output-module
	      (let ((output-getter (make-get-output-lambda output-module output-socket)))
		(push (lambda()
			(funcall input-setter (funcall output-getter)))
		      input-setters))
	      (push (lambda() (funcall input-setter nil)) input-setters)))))
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

