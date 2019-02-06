(in-package :cl-synthesizer)

;;
;;
;; Rack
;;
;;

(defun get-environment (rack)
  "Returns the environment of the rack."
  (getf rack :environment))

(defun get-module (rack name)
  "Get a module of a rack. The function has the following arguments:
    <ul>
      <li>rack The rack.</li>
      <li>name The name of the module</li>
    </ul>
   Returns the module (represented as a property list) or nil if a module
   with the given name has not been added to the rack."
  (funcall (getf rack :get-module-by-name) name))

(defun find-module (rack module-path)
  "Get a module of a rack. The function has the following arguments:
    <ul>
      <li>rack The root rack.</li>
      <li>module-path The path of the module within the rack (through multiple nested racks).</br>
         Example 1: \"VCO\"</br> 
         Example 2: '(\"VOICE-1\" \"VCO\")</li>
    </ul>
   Returns nil or a values object consisting of the rack of the module, the module name and the module itself."
  (if (not (listp module-path))
      (setf module-path (list module-path)))
  (if (not module-path)
      (values nil nil nil)
      (let* ((module (funcall (getf rack :get-module-by-name) (first module-path))))
	(if module
	    (let ((module module))
	      (if (< 1 (length module-path))
		  (if (getf module :is-rack)
		      (find-module module (rest module-path))
		      (values nil nil nil))
		  (values rack (first module-path) module)))
	    (values nil nil nil)))))

(defun add-hook (rack hook)
  "Adds a hook to the rack. A hook is called each time after the rack has updated its state.
   A hook consists a property list with the following keys:
   <ul>
      <li>:update A function with no arguments that is called after the rack has updated its state.</li>
      <li>:shutdown A function with no arguments that is called when the rack is shutting down.</li>
   </ul>
   Hooks must not modify the rack. See also <b>cl-synthesizer-monitor:add-monitor</b>."
  (funcall (getf rack :add-hook) hook))

(defun add-module (rack module-name module-fn &rest args)
  "Adds a module to a rack. The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
	<li>module-name Unique name of the module, for example \"VCO-1\". If the name
	    is already used by another module an assembly-error is signalled.</li>
	<li>module-fn A function that instantiates the module. This function is
	    called by the rack with the following arguments:
	    <ul>
		<li>name Name of the module.</li>
		<li>environment The synthesizer environment.</li>
		<li>module-args Any additional arguments passed to add-module.</li>
	    </ul>
	    The module instantiation function must return a property list with the following keys:
	    <ul>
		<li>:inputs A function with no arguments that returns a list of keywords that represent the
		    input sockets exposed by the module.</li>
		<li>:outputs A function with no arguments that returns a list of keywords that represent the
		    output sockets exposed by the module.</li>
		<li>:update A function that is called with the values of the modules input sockets
                    in order to update the state of the module (the state of its output sockets).
		    All input parameters are passed as a single argument which consists of a property list or
                    nil if the module does not expose any inputs. To avoid excessive consing this
                   list is allocated on instantiation of the module and then used for all update calls
                   of the module.</li>
		<li>:get-output A function that is called in order to get the value of a specific
		    output socket. The function is called with a keyword that identifies the output socket
		    whose state is to be returned. The function must not modify the value
		    of the given or any other output socket.</li>
		<li>:shutdown An optional function with no arguments that is called when the rack
		    is shutting down.</li>
                <li>:get-state An optional function with which an internal state of a module can
                    be exposed, for example a VCO may expose its frequency. The function has one 
                    argument that consists of a keyword identifying the requested state, for 
                    example :frequency.</li>
	    </ul>
            <p>
	    A module must not add or remove input/output sockets after it has been instantiated.
            </p>
	</li>
	<li>&rest args Arbitrary additional arguments to be passed to the module instantiation function.
	    These arguments typically consist of keyword parameters.</li>
    </ul>"
  (if (funcall (getf rack :get-module-by-name) module-name)
      (signal-assembly-error
       :format-control "A module with name ~a has already been added to the rack"
       :format-arguments (list module-name)))
  (let ((environment (getf rack :environment)))
    (let ((module (apply module-fn `(,module-name ,environment ,@args))))
      (dolist (property '(:inputs :outputs :update :get-output))
	(if (not (functionp (getf module property)))
	    (signal-assembly-error
	     :format-control "Invalid module ~a: Property ~a must be a function but is ~a"
	     :format-arguments (list module-name property (getf module property)))))
      (funcall (getf rack :add-module) module module-name)
      module)))

(defun make-rack (&key environment (input-sockets nil) (output-sockets nil))
  "Creates a rack. A rack is a module container and also a module, which means that racks 
   can be added to other racks. The function has the following arguments:
    <ul>
	<li>:environment The synthesizer environment.</li>
        <li>:input-sockets The input sockets to be exposed by the rack. The inputs
        can be accessed for patching of inner modules of the rack via the virtual 
        module \"INPUT\".</li>
        <li>:output-sockets The output sockets to be exposed by the rack. The outputs
        can be accessed for patching of inner modules of the rack via the virtual 
        module \"OUTPUT\".</li>
    </ul>
    <p>    
    The update function of the rack calls the update function of all modules that have
    been added to the rack. If the rack has already been shut down it immediately returns <b>nil</b>.
    Othwerwise it returns <b>t</b>.
    </p><p>
    The shutdown function shuts the rack down by calling the shutdown handlers of all modules 
    and hooks of the rack. If the rack has already been shut down the function does not call any handlers.
    </p>
    <p>See also: add-module</p>"
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (if (not environment)
      (signal-invalid-arguments-error
       :format-control "Environment must not be nil"
       :format-arguments nil))

  (let ((has-shut-down nil)
	(input-rm nil) (inputs nil) (output-rm nil) (outputs nil)
	(modules nil) ;; list of (:module module :name name)
	(hooks nil)
	(patches nil) ;; list of (:output-name "name" :output-socket <socket> :input-name "name" :input-socket <socket>)
	(compiled-rack nil))

    (labels (
	     (get-module-name (module)
	       (let ((match (find-if (lambda (cur-module) (eq module (getf cur-module :module)))
				     modules)))
		 (if match (getf match :name) nil)))
	     (get-module-by-name (name)
	       (let ((module
		      (find-if (lambda (m) (string= name (getf m :name)))
			       modules)))
		 (if module (getf module :module) nil)))
	     (get-module-input-patches (module)
	       "returns sparse list of (input-socket output-module output-socket)"
	       (let* ((result nil)
		      (name (get-module-name module)))
		 (dolist (input-socket (funcall (getf module :inputs)))
		   (let ((patch
			  (find-if
			   (lambda (p) (and
					(string= (getf p :input-name) name)
					(eq (getf p :input-socket) input-socket)))
			   patches)))
		     (if patch
			 (push (list input-socket
				     (get-module-by-name (getf patch :output-name))
				     (getf patch :output-socket)) result)
			 (push (list input-socket nil nil) result))))
		 result)))
      (flet ((compile-rack ()
	       ;; Build module trace
	       (let ((ordered-modules nil) (visited-modules nil))
		 ;; Mark INPUT bridge module as visited
		 (push (get-module-by-name "INPUT") visited-modules)
		 (labels ((traverse-module (module)
			    (if (not (find module visited-modules :test #'eq))
				(progn
				  (push module visited-modules)
				  (dolist (binding (get-module-input-patches module))
				    (let ((output-module (second binding)))
				      (if output-module
					  (traverse-module output-module))))
				  (push module ordered-modules)))))
		   (dolist (module modules)
		     (traverse-module (getf module :module)))
		   (flet ((compile-update-module (module)
			    "Compile update-module by collecting all inputs and generating getter functions"
			    (let ((input-args nil) (input-getters nil)
				  (module-update-fn (getf module :update)))
			      ;; Prepare static input property list with which
			      ;; the update function of the module will be called.
			      ;; (:INPUT-1 nil :INPUT-2 nil ...)
			      (dolist (input-socket (funcall (getf module :inputs)))
				(push nil input-args)
				(push input-socket input-args))
			      ;; Push getters for all inputs
			      (dolist (binding (get-module-input-patches module))
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
			      ;; The compiled updated function
			      (lambda ()
				(dolist (fn input-getters)
				  (funcall fn))
				(funcall module-update-fn input-args)))))
		     ;; Compile
		     (let ((lambdas nil))
		       (dolist (module ordered-modules)
			 (push (compile-update-module module) lambdas))
		       ;; The compiled update function
		       (lambda (args)
			 ;; Set outputs of INPUT bridge module
			 (setf inputs args)
			 ;; Update modules (lambdas are already ordered due to two previous push cycles)
			 (dolist (fn lambdas)
			   (funcall fn))
			 ;; Call hooks
			 (dolist (h hooks)
			   (funcall (getf h :update))))))))))
	(let ((rack
	       (list
		:update (lambda (args)
			  (if has-shut-down
			      nil
			      (progn
				(if (not compiled-rack)
				    (setf compiled-rack (compile-rack)))
				(funcall compiled-rack args)
				t)))
		:get-output (lambda (socket) (getf outputs socket))
		;; TODO Consider if we should expose this function
		:modules (lambda() (mapcar (lambda (m) (getf m :module)) modules))
		:get-module-name (lambda (module) (get-module-name module))
		:get-module-by-name (lambda(name) (get-module-by-name name))
		:outputs (lambda() output-sockets)
		:inputs (lambda() input-sockets)
		:add-module (lambda (module module-name)
				   (push (list :module module :name module-name) modules)
				   )
		:add-hook (lambda (hook) (push hook hooks))
		:shutdown (lambda()
			    (if (not has-shut-down)
				(progn
				  (setf has-shut-down t)
				  (dolist (module modules)
				    (let ((f (getf module :shutdown)))
				      (if f (funcall f))))
				  (dolist (m hooks)
				    (let ((h (getf m :shutdown)))
				      (if h (funcall h)))))))
		:environment environment
		:is-rack t
		:add-patch (lambda (&key output-name output-socket input-name input-socket)
			     (push (list
				    :output-name output-name
				    :output-socket output-socket
				    :input-name input-name
				    :input-socket input-socket)
				   patches))
		:get-module-input-patches
		(lambda (rm)
		  (let ((name (get-module-name rm)) (found-patches nil))
		    (dolist (patch patches)
		      (if (string= name (getf patch :input-name))
			  (push patch found-patches)))
		    found-patches))

		:get-module-input-patch
		(lambda (module input-socket)
		  (let* ((name (get-module-name module))
			 ;; list of (:output-name "name" :output-socket <socket> :input-name "name" :input-socket <socket>)
			 (patch (find-if (lambda (p) (and (string= name (getf p :input-name))
					      (eq input-socket (getf p :input-socket))))
					 patches)))
		    patch))
		:get-module-output-patch
		(lambda (module output-socket)
		  (let* ((name (get-module-name module))
			 ;; list of (:output-name "name" :output-socket <socket> :input-name "name" :input-socket <socket>)
			 (patch (find-if (lambda (p) (and (string= name (getf p :output-name))
					      (eq output-socket (getf p :output-socket))))
					 patches)))
		    patch))
		    
		
		)))
	  ;;
	  ;; Add bridge modules
	  ;;
	  (setf input-rm
		(add-module rack "INPUT"
			    (lambda(name environment)
			      (declare (ignore name environment))
			      (list
			       :inputs (lambda() nil)
			       :outputs (lambda() input-sockets)
			       :update (lambda (args) (declare (ignore args)) nil)
			       :get-output (lambda(socket) (getf inputs socket))))))
	  
	  (setf output-rm
		(add-module rack "OUTPUT"
			    (lambda(name environment)
			      (declare (ignore name environment))
			      (list
			       :inputs (lambda() output-sockets)
			       :outputs (lambda() nil)
			       :update (lambda (args) (setf outputs args))
			       :get-output (lambda(socket) (getf outputs socket))))))

	  rack)))))

(defun add-patch (rack source-rm-name source-output-socket destination-rm-name destination-input-socket)
  "Adds a patch to the rack. A patch is an unidirectional connection between an output socket
    of a source module and an input socket of a destination module. The rack supports cycles 
    which means that an output socket of a module can be patched with one of its inputs (typically via
    multiple hops through other modules). The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
	<li>source-rm-name Name of the source module.</li>
	<li>source-output-socket A keyword representing one of the output sockets of the
	    source module.</li>
	<li>destination-rm-name Name of the destination module.</li>
	<li>destination-input-socket A keyword representing one of the input sockets of the
	    destination module.</li>
    </ul>
    The rack signals an assembly-error in the following cases:
    <ul>
	<li>A module with the given source name does not exist.</li>
	<li>A module with the given destination name does not exist.</li>
	<li>The given source-output-socket is already connected with a module</li>
	<li>The given source-output-socket is not exposed by the source module.</li>
	<li>The given destination-input-socket is already connected with a module.</li>
	<li>The given destination-input-socket is not exposed by the destination module.</li>
    </ul>"
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((source-rm (funcall (getf rack :get-module-by-name) source-rm-name))
	(destination-rm (funcall (getf rack :get-module-by-name) destination-rm-name)))
    (if (not source-rm)
	(signal-assembly-error
	 :format-control "Cannot find source module ~a"
	 :format-arguments (list source-rm-name)))
    (if (not destination-rm)
	(signal-assembly-error
	 :format-control "Cannot find destination module ~a"
	 :format-arguments (list destination-rm-name)))
    (if (not (find source-output-socket (funcall (getf source-rm :outputs))))
	(signal-assembly-error
	 :format-control "Module ~a does not have output socket ~a"
	 :format-arguments (list source-rm-name source-output-socket)))
    (if (not (find destination-input-socket (funcall (getf destination-rm :inputs))))
	(signal-assembly-error
	 :format-control "Module ~a does not have input socket ~a"
	 :format-arguments (list destination-rm-name destination-input-socket)))
    ;; (:output-name "name" :output-socket <socket> :input-name "name" :input-socket <socket>)
    (let ((p (funcall (getf rack :get-module-input-patch) destination-rm destination-input-socket)))
      (if p
	  (signal-assembly-error
	   :format-control "Input socket ~a of module ~a is already connected to output socket ~a of module ~a"
	   :format-arguments (list
			      destination-input-socket
			      destination-rm-name
			      (getf p :output-name)
			      (getf p :output-socket)))))
    
    (let ((p (funcall (getf rack :get-module-output-patch) source-rm source-output-socket)))
      (if p
	  (signal-assembly-error
	   :format-control "Output socket ~a of module ~a is already connected to input socket ~a of module ~a"
	   :format-arguments (list
			      source-output-socket
			      source-rm-name
			      (getf p :input-name)
			      (getf p :input-socket)))))

    (funcall (getf rack :add-patch)
	     :output-name source-rm-name
	     :output-socket source-output-socket
	     :input-name destination-rm-name
	     :input-socket destination-input-socket)))

(defun get-patch (rack module-name socket-type socket)
  "Returns the destination module and input/output socket, to which a given
    source module and one if its input/output sockets is connected.
    The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
	<li>module-name Name of the source module.</li>
	<li>socket-type :input-socket if the patch of an input socket is required or
	    :output-socket for the patch of an output socket of the source module.</li>
	<li>socket A keyword identifying an input or output socket of the source module.</li>
    </ul>
    The function returns returns a values object with the following entries:
    <ul>
	<li>name Name of the destination module.</li>
	<li>module The destination module represented as a property list.</li>
	<li>socket A keyword that identifies the input or output socket of the destination
	    module. If the socket type of the source module is :input-socket then this
	    keyword represents an output socket of the destination module. Otherwise
	    it represents an input socket.
	</li>
    </ul>
    If the module does not exist, the module does not expose the given socket, or
    if the socket is not patched, all entries of the returned values object are nil."
  (if (not (or (eq :input-socket socket-type) (eq :output-socket socket-type)))
      (signal-invalid-arguments-error
       :format-control "get-patch: socket must be one of :input-socket or :output-socket"
       :format-arguments nil))
  (let ((rm (funcall (getf rack :get-module-by-name) module-name)))
    (if (not rm)
	(values nil nil nil)
	(if (eq :input-socket socket-type)
	    (let ((patch (funcall (getf rack :get-module-input-patch) rm socket)))
	      (if (not patch)
		  (values nil nil nil)
		  (values
		   (getf patch :output-name)
		   (funcall (getf rack :get-module-by-name) (getf patch :output-name))
		   (getf patch :output-socket))))
	    (let ((patch (funcall (getf rack :get-module-output-patch) rm socket)))
	      (if (not patch)
		  (values nil nil nil)
		  (values
		   (getf patch :input-name)
		   (funcall (getf rack :get-module-by-name) (getf patch :input-name))
		   (getf patch :input-socket))))))))


(defun get-rack-info (rack)
  (let ((module-count 0) (patch-count 0))
    ;; Added modules + INPUT + OUTPUT
    (dolist (module (funcall (getf rack :modules)))
      (setf module-count (+ module-count 1))
      (setf patch-count (+ patch-count (length (funcall (getf rack :get-module-input-patches) module))))
      (let ((module module))
	(if (getf module :is-rack)
	    (let ((info (get-rack-info module)))
	      (setf module-count (+ module-count (getf info :module-count)))
	      (setf patch-count (+ patch-count (getf info :patch-count)))))))
    (list
     :module-count module-count
     :patch-count patch-count)))

