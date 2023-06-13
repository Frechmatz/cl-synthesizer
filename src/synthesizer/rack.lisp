(in-package :cl-synthesizer)

(defun make-rack (&key environment)
  "Creates a rack.<p>The function has the following parameters:
    <ul>
	<li>:environment The environment.</li>
    </ul></p>
    Returns the rack."
  (if (not environment)
      (error
       'simple-error
       :format-control "Environment must not be nil"
       :format-arguments nil))

  (let ((this nil) ;; Property-List representation of the rack
	(has-shut-down nil)
	;; list of modules
	(modules nil)
	;; list of (:updated <updated-fn> :updating <updating-fn> :shutdown <shutdown-fn>)
	(hooks nil)
	;; list of (:output-name "name" :output-socket <socket> :input-name "name" :input-socket <socket>)
	(patches nil)
	(compiled-rack nil)
	;; List of (:rack-socket s :module-name module :module-socket module-socket)
	(exposed-input-sockets nil)
	;; List of (:rack-socket s :module-name module :module-socket module-socket)
	(exposed-output-sockets nil)
	(rack-outputs nil)
	(rack-inputs nil))
    
    (labels
	;;
	;;
	;;
	((get-module (path)
	   (if (not (listp path))
	       (find-if
		(lambda (m) (string= path (funcall (getf m :cl-synthesizer-module-get-name))))
		modules)
	       (let ((rack this) (module nil))
		 (dolist (path-segment path)
		   (if (not rack)
		       (setf module nil)
		       (setf module (funcall (getf rack :get-module) path-segment)))
		   (if (and module (getf module :is-rack))
		       (setf rack module)
		       (setf rack nil)))
		 module)))
	 ;;
	 ;;
	 ;;
	 (update-rack-inputs ()
	   (labels ((get-input-setter-fn (module-name socket)
		      (getf (getf (funcall (getf (get-module module-name) :inputs))
				  socket) :set))
		    (get-input-getter-fn (module-name socket)
		      (getf (getf (funcall (getf (get-module module-name) :inputs))
				  socket) :get)))
	     (setf rack-inputs nil)
	     (dolist (exposed-input-socket exposed-input-sockets)
	       (push (list
		      :set (get-input-setter-fn
			    (getf exposed-input-socket :module-name)
			    (getf exposed-input-socket :module-socket))
		      :get (get-input-getter-fn
			    (getf exposed-input-socket :module-name)
			    (getf exposed-input-socket :module-socket)))
		     rack-inputs)
	       (push (getf exposed-input-socket :rack-socket) rack-inputs))))
	 ;;
	 ;;
	 ;;
	 (update-rack-outputs ()
	   (labels ((get-output-getter-fn (module-name socket)
		      (getf (getf (funcall (getf (get-module module-name) :outputs))
				  socket) :get)))
	     (setf rack-outputs nil)
	     (dolist (exposed-output-socket exposed-output-sockets)
	       (push
		(list :get (get-output-getter-fn
			    (getf exposed-output-socket :module-name)
			    (getf exposed-output-socket :module-socket)))
		rack-outputs)
	       (push (getf exposed-output-socket :rack-socket) rack-outputs))))
	 ;;
	 ;;
	 ;;
	 (update ()
	   (if has-shut-down
	       nil
	       (progn
		 (if (not compiled-rack)
		     (setf
		      compiled-rack
		      (cl-synthesizer-rack-compiler:compile-rack this)))
		 (funcall compiled-rack)
		 t)))
	 ;;
	 ;;
	 ;;
	 (assert-plistp (plist format-control format-arguments)
	   (if (or (not (listp plist)) (not (evenp (length plist))))
	       (error
		'assembly-error
		:format-control format-control
		:format-arguments format-arguments))
	   (cl-synthesizer-property-list-iterator:do-property-list-keys
	       plist
	       keyword
	     (if (not (keywordp keyword))
		 (error
		  'assembly-error
		  :format-control format-control
		  :format-arguments format-arguments))))
	 ;;
	 ;;
	 ;;
	 (add-hook (hook)
	   (assert-plistp hook "Hook is not a property list: ~a" (list hook))
	   (setf compiled-rack nil)
	   (push hook hooks))
	 ;;
	 ;;
	 ;;
	 (shutdown ()
	   (if (not has-shut-down)
	       (progn
		 (setf has-shut-down t)
		 (dolist (module modules)
		   (let ((fn (getf module :shutdown)))
		     (if fn (funcall fn))))
		 (dolist (hook hooks)
		   (let ((fn (getf hook :shutdown)))
		     (if fn (funcall fn)))))))
	 ;;
	 ;;
	 ;;
	 (assert-add-patch (output-name output-socket input-name input-socket
			    exposed-input-sockets exposed-output-sockets)
	   (labels ((is-module-input-exposed-as-input-socket (module-name socket)
		      (find-if
		       (lambda(entry)
			 (and
			  (eq socket (getf entry :module-socket))
			  (string= module-name (getf entry :module-name))))
		       exposed-input-sockets))
		    (is-module-output-exposed-as-output-socket (module-name socket)
		      (find-if
		       (lambda(entry)
			 (and
			  (eq socket (getf entry :module-socket))
			  (string= module-name (getf entry :module-name))))
		       exposed-output-sockets)))
	     
	     (let ((source-module (get-module output-name))
		   (destination-module (get-module input-name)))
	       (if (not source-module)
		   (error
		    'assembly-error
		    :format-control "add-patch: Cannot find output module '~a'"
		    :format-arguments (list output-name)))
	       (if (not destination-module)
		   (error
		    'assembly-error
		    :format-control "add-patch: Cannot find input module '~a'"
		    :format-arguments (list input-name)))
	       (if (not (find output-socket (funcall (getf source-module :outputs))))
		   (error
		    'assembly-error
		    :format-control "add-patch: Module '~a' does not expose output socket '~a'"
		    :format-arguments (list output-name output-socket)))
	       (if (not (find input-socket (funcall (getf destination-module :inputs))))
		   (error
		    'assembly-error
		    :format-control "add-patch: Module '~a' does not expose input socket '~a'"
		    :format-arguments (list input-name input-socket)))
	       (let ((p (find-if
			 (lambda (p)
			   (and (string= input-name (getf p :input-name))
				(eq input-socket (getf p :input-socket))))
			 patches)))
		 (if p (error
			'assembly-error
			:format-control
			"add-patch: Input socket '~a' of module '~a' is already connected with output socket '~a' of module '~a'"
			:format-arguments (list
					   input-socket
					   input-name
					   (getf p :output-name)
					   (getf p :output-socket)))))
	       (let ((p (find-if
			 (lambda (p)
			   (and (string= output-name (getf p :output-name))
				(eq output-socket (getf p :output-socket))))
			 patches)))
		 (if p (error
			'assembly-error
			:format-control
			"add-patch: Output socket '~a' of module '~a' is already connected with input socket '~a' of module '~a'"
			:format-arguments
			(list
			 output-socket
			 output-name
			 (getf p :input-socket)
			 (getf p :input-name)))))
	       
	       (if (is-module-input-exposed-as-input-socket input-name input-socket)
		   (error
		    'assembly-error
		    :format-control
		    "add-patch: Module '~a' Input socket '~a' is exposed as rack input"
		    :format-arguments (list input-name input-socket)))
	       (if (is-module-output-exposed-as-output-socket output-name output-socket)
		   (error
		    'assembly-error
		    :format-control
		    "add-patch: Module '~a' Output socket '~a' is exposed as rack output"
		    :format-arguments (list input-name input-socket))))))
	 ;;
	 ;;
	 ;;
	 (add-patch (output-name output-socket input-name input-socket)
	   (assert-add-patch
	    output-name output-socket
	    input-name input-socket
	    exposed-input-sockets
	    exposed-output-sockets)
	   (setf compiled-rack nil)
	   (push (list
		  :output-name output-name
		  :output-socket output-socket
		  :input-name input-name
		  :input-socket input-socket)
		 patches))
	 ;;
	 ;;
	 ;;
	 (assert-add-rack-input (rack-input-socket input-module-name
				 input-socket exposed-input-sockets)
	   (labels ((get-exposed-input-socket (socket)
		      (find-if
		       (lambda(entry)
			 (eq socket (getf entry :rack-socket)))
		       exposed-input-sockets))
		    (is-input-patched (module-name socket)
		      (find-if
	      (lambda (p)
		(and
		 (eq socket (getf p :input-socket))
		 (string= module-name (getf p :input-name))))
	      patches)))
	     
	     (if (get-exposed-input-socket rack-input-socket)
		 (error
		  'assembly-error
		  :format-control
		  "add-rack-input: Module already exposes input socket '~a'"
		  :format-arguments (list rack-input-socket)))
	     (let ((module (get-module input-module-name)))
	       (if (not module)
		   (error
		    'assembly-error
		    :format-control
		    "add-rack-input: Cannot find module '~a'"
		    :format-arguments (list input-module-name)))
	       (if (not (find input-socket (funcall (getf module :inputs))))
		   (error
		    'assembly-error
		    :format-control
		    "add-rack-input: Module '~a' does not expose input socket '~a'"
		    :format-arguments (list input-module-name input-socket)))
	       (if (is-input-patched input-module-name input-socket)
		   (error
		    'assembly-error
		    :format-control
		    "add-rack-input: Module '~a' Input Socket '~a' already patched"
		    :format-arguments (list input-module-name input-socket))))))
	 ;;
	 ;;
	 ;;
	 (assert-add-rack-output (rack-output-socket output-module-name
				  output-socket exposed-output-sockets)
	   (labels ((get-exposed-output-socket (socket)
		      (find-if
		       (lambda(entry)
			 (eq socket (getf entry :rack-socket)))
		       exposed-output-sockets))
		    (is-output-patched (module-name socket)
		      (find-if
		       (lambda (p)
			 (and
			  (eq socket (getf p :output-socket))
			  (string= module-name (getf p :output-name))))
		       patches)))
	     
	     (if (get-exposed-output-socket rack-output-socket)
		 (error
		  'assembly-error
		  :format-control
		  "add-rack-outut: Module already exposes output socket '~a'"
		  :format-arguments (list rack-output-socket)))
	     (let ((module (get-module output-module-name)))
	       (if (not module)
		   (error
		    'assembly-error
		    :format-control
		    "add-rack-output: Cannot find module '~a'"
		    :format-arguments (list output-module-name)))
	       (if (not (find output-socket (funcall (getf module :outputs))))
		   (error
		    'assembly-error
		    :format-control
		    "add-rack-output: Module '~a' does not expose output socket '~a'"
		    :format-arguments (list output-module-name output-socket)))
	       (if (is-output-patched output-module-name output-socket)
		   (error
		    'assembly-error
		    :format-control
		    "add-rack-output: Module '~a' Output Socket '~a' already patched"
		    :format-arguments (list output-module-name output-socket))))))
	 ;;
	 ;;
	 ;;
	 (assert-add-module (module-name)
	   (if (get-module module-name)
	       (error
		'assembly-error
		:format-control
		"add-module: A module with name '~a' has already been added to the rack"
		:format-arguments (list module-name))))
	 ;;
	 ;;
	 ;;
	 (add-rack-output (rack-output-socket output-module-name output-socket)
	   (assert-add-rack-output
	    rack-output-socket
	    output-module-name
	    output-socket
	    exposed-output-sockets)
	   (push (list
		  :rack-socket rack-output-socket
		  :module-name output-module-name
		  :module-socket output-socket)
		 exposed-output-sockets)
	   (update-rack-outputs))
	 ;;
	 ;;
	 ;;
	 (add-rack-input (rack-input-socket input-module-name input-socket)
	   (assert-add-rack-input
	    rack-input-socket
	    input-module-name
	    input-socket
	    exposed-input-sockets)
	   (push (list
		  :rack-socket rack-input-socket
		  :module-name input-module-name
		  :module-socket input-socket)
		 exposed-input-sockets)
	   (update-rack-inputs))
	 ;;
	 ;;
	 ;;
	 (assert-module-structure (module-name module)
	   (assert-plistp module "Module ~a is not a property list: ~a" (list module-name module))
	   ;; Check module update function
	   (if (not (functionp (getf module :update)))
	       (error
		'assembly-error
		:format-control "Invalid module '~a': Property :update must be a function"
		:format-arguments (list module-name)))
	   ;;
	   ;; Check module inputs
	   ;;
	   (if (not (functionp (getf module :inputs)))
	       (error
		'assembly-error
		:format-control "Invalid module '~a': Property :inputs must be a function"
		:format-arguments (list module-name)))
	   (let ((inputs (funcall (getf module :inputs))))
	     ;; check that the inputs are represented by a property list
	     (assert-plistp
	      inputs
	      "Inputs of module ~a are not a property list: ~a"
	      (list module-name inputs))
	     ;; check all inputs
	     (cl-synthesizer-property-list-iterator:do-property-list
		 inputs
		 socket
		 socket-properties
	       (assert-plistp
		socket-properties
		"Socket properties of module ~a input socket ~a are not a property list: ~a"
		(list module-name socket socket-properties))
	       ;; check presence of setter
	       (if (not (getf socket-properties :set))
		   (error
		    'assembly-error
		    :format-control "Input socket ~a of module ~a does not have a 'set' function"
		    :format-arguments (list socket module-name)))
	       ;; check if setter is a function
	       (if (not (functionp (getf socket-properties :set)))
		   (error
		    'assembly-error
		    :format-control "Setter of input socket ~a of module ~a is not a function"
		    :format-arguments (list socket module-name)))
	       ;; check presence of getter
	       (if (not (getf socket-properties :get))
		   (error
		    'assembly-error
		    :format-control "Input socket ~a of module ~a does not have a 'get' function"
		    :format-arguments (list socket module-name)))
	       ;; check if getter is a function
	       (if (not (functionp (getf socket-properties :get)))
		   (error
		    'assembly-error
		    :format-control "Getter of input socket ~a of module ~a is not a function"
		    :format-arguments (list socket module-name)))))
	   ;;
	   ;; Check module outputs
	   ;;
	   (if (not (functionp (getf module :outputs)))
	       (error
		'assembly-error
		:format-control "Invalid module '~a': Property :outputs must be a function"
		:format-arguments (list module-name)))
	   (let ((outputs (funcall (getf module :outputs))))
	     ;; check that the outputs are represented by a property list
	     (assert-plistp
	      outputs
	      "Outputs of module ~a are not a property list: ~a"
	      (list module-name outputs))
	     ;; check all outputs
	     (cl-synthesizer-property-list-iterator:do-property-list
		 outputs
		 socket
		 socket-properties
	       (assert-plistp
		socket-properties
		"Socket properties of module ~a output socket ~a are not a property list: ~a"
		(list module-name socket socket-properties))
	       ;; check presence of getter
	       (if (not (getf socket-properties :get))
		   (error
		    'assembly-error
		    :format-control "Output socket ~a of module ~a does not have a 'get' function"
		    :format-arguments (list socket module-name)))
	       ;; check if getter is a function
	       (if (not (functionp (getf socket-properties :get)))
		   (error
		    'assembly-error
		    :format-control "Getter of output socket ~a of module ~a is not a function"
		    :format-arguments (list socket module-name))))))
	 ;;
	 ;;
	 ;;
	 (enrich-module (module module-name)
	   ;; Verify that module does not use reserved properties
	   (dolist (keyword (list :cl-synthesizer-module-get-name :cl-synthesizer-module-get-rack))
	     (if (find keyword module)
		 (error
		  'assembly-error
		  :format-control
		  "Invalid module '~a': Property '~a' is a reserved for internal use"
		  :format-arguments
		  (list module-name keyword))))
	   (let ((enriched-module
		   (concatenate
		    'list 
		    module
		    (list
		     :cl-synthesizer-module-get-name (lambda() module-name)
		     :cl-synthesizer-module-get-rack (lambda() this)))))
	     enriched-module))
	 ;;
	 ;;
	 ;;
	 (add-module (module-name module-fn &rest args)
	   (assert-add-module module-name)
	   (let ((module (apply module-fn `(,module-name ,environment ,@args))))
	     (assert-module-structure module-name module)
	     (setf module (enrich-module module module-name))
	     (setf compiled-rack nil)
	     (push module modules)
	     module)))
      (let ((rack
	      (list
	       :get-module (lambda (name) (get-module name))
	       :add-rack-input (lambda(rack-input-socket input-module-name input-socket)
				 (add-rack-input rack-input-socket input-module-name input-socket))
	       :add-rack-output (lambda(rack-output-socket output-module-name output-socket)
				  (add-rack-output rack-output-socket output-module-name output-socket))
	       :modules (lambda() modules)
	       :outputs (lambda() rack-outputs)
	       :inputs (lambda() rack-inputs)
	       :patches (lambda() patches)
	       :hooks (lambda () hooks)
	       :update (lambda () (update))
	       :add-module (lambda (module-name module-fn &rest args)
			     (apply #'add-module module-name module-fn args))
	       :add-hook (lambda (hook) (add-hook hook))
	       :shutdown (lambda() (shutdown))
	       :environment environment
	       :is-rack t
	       :add-patch (lambda (output-name output-socket input-name input-socket)
			    (add-patch output-name output-socket input-name input-socket)))))
      	(setf this rack)
	rack))))

