;;
;; Rack compiler
;;

(in-package :cl-synthesizer-rack-compiler)

(defun get-input-sockets (module)
  "TODO Inefficient implementation, but for now live with it"
  (let ((sockets nil) (counter 0))
    (dolist (input (funcall (getf module :inputs)))
      ;; Property list. Sockets are at indices 0, 2, 4, 6, ...
      (if (= (rem counter 2) 0)
	  (push input sockets))
      (setf counter (+ counter 1)))
    sockets))

(defun get-module-input-patches (rack module)
  (let ((result nil) (name (cl-synthesizer:get-module-name rack module)))
    (dolist (input-socket (get-input-sockets module))
      (let ((patch
	     (find-if
	      (lambda (p)
		(and
		 (string= (getf p :input-name) name)
		 (eq (getf p :input-socket) input-socket)))
	      (cl-synthesizer:get-patches rack))))
	(if patch
	    (push (list
		   input-socket
		   (cl-synthesizer:get-module rack (getf patch :output-name))
		   (getf patch :output-socket)) result))))
    result))

(defun get-module-trace (rack)
  "Get list of modules in execution order"
  (let ((module-trace nil)
	(visited-modules nil))
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
  (getf (getf (funcall (getf module :outputs)) output-socket) :get))

(defun compile-module (rack module)
  (let ((input-setters nil)
	(inputs (funcall (getf module :inputs)))
	(module-update-fn (getf module :update)))
    ;; Push setters for all inputs
    (dolist (binding (get-module-input-patches rack module))
      (let ((cur-input-socket (first binding))
	    (output-module (second binding))
	    (output-socket (third binding)))
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
  (let ((lambdas (mapcar (lambda (module) (compile-module rack module)) (get-module-trace rack))))
    (lambda ()
      ;; Update modules
      (dolist (fn lambdas)
	(funcall fn))
      ;; Call hooks
      (dolist (h (cl-synthesizer:get-hooks rack))
	(cl-synthesizer:update h)))))

