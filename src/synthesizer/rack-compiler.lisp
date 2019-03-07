;;
;; Rack compiler
;;
;; does only depend on the functions exposed by the "rack" property list
;;

(in-package :cl-synthesizer-rack-compiler)

(defun get-module-name (rack module)
  (let ((match
	    (find-if
	     (lambda (cur-module) (eq module (getf cur-module :module)))
	     (funcall (getf rack :modules)))))
    (if match (getf match :name) nil)))

(defun get-module-by-name (rack name)
  (let ((module
	 (find-if
	  (lambda (m) (string= name (getf m :name)))
	  (funcall (getf rack :modules)))))
    (if module (getf module :module) nil)))

(defun get-module-input-patches (rack module)
  "Returns a sparse list of (input-socket output-module output-socket)"
  (let ((result nil) (name (get-module-name rack module)))
    (dolist (input-socket (funcall (getf module :inputs)))
      (let ((patch
	     (find-if
	      (lambda (p)
		(and
		 (string= (getf p :input-name) name)
		 (eq (getf p :input-socket) input-socket)))
	      (funcall (getf rack :patches)))))
	(if patch
	    (push (list
		   input-socket
		   (get-module-by-name rack (getf patch :output-name))
		   (getf patch :output-socket)) result)
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
      (dolist (module (funcall (getf rack :modules)))
	(traverse-module (getf module :module)))
      (nreverse module-trace))))

(defun compile-module (rack module)
  "Compile module"
  (let ((input-args nil) (input-getters nil)
	(module-update-fn (getf module :update)))
    ;; Prepare static input property list with which
    ;; the update function of the module will be called.
    ;; (:INPUT-1 nil :INPUT-2 nil ...)
    (dolist (input-socket (funcall (getf module :inputs)))
      (push nil input-args)
      (push input-socket input-args))
    ;; Push getters for all inputs
    (dolist (binding (get-module-input-patches rack module))
      (let ((cur-input-socket (first binding))
	    (output-module (second binding))
	    (output-socket (third binding)))
	(if output-module
	    (let ((get-output-fn (getf output-module :get-output)))
	      (push (lambda()
		      ;; Get output value from input module
		      (setf (getf input-args cur-input-socket)
			    (funcall get-output-fn output-socket)))
		    input-getters))
	    (push (lambda()
		    (setf (getf input-args cur-input-socket) nil))
		  input-getters))))
    ;; The compiled update function
    (lambda ()
      ;; Collect input values
      (dolist (fn input-getters)
	(funcall fn))
      ;; Update module
      (funcall module-update-fn input-args))))

(defun compile-rack (rack)
  "Compile a rack. Returns a function to be called with the values of the input sockets of the rack."
  (let ((input-bridge-module-update-fn (getf (get-module-by-name rack "INPUT") :update))
	(lambdas (mapcar (lambda (module) (compile-module rack module)) (get-module-trace rack))))
    (lambda (args)
      ;; Update INPUT bridge module
      (funcall input-bridge-module-update-fn args)
      ;; Update modules
      (dolist (fn lambdas)
	(funcall fn))
      ;; Call hooks
      (dolist (h (funcall (getf rack :hooks)))
	(funcall (getf h :update))))))
