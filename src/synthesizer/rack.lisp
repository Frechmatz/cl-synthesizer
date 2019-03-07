(in-package :cl-synthesizer)

;;
;;
;; Rack
;;
;;

(defun make-rack (&key environment (input-sockets nil) (output-sockets nil))
  "Creates a rack. A rack is a module container as well as a module. Racks can
   be added to other racks. The function has the following arguments:
    <ul>
	<li>:environment The synthesizer environment.</li>
        <li>:input-sockets The input sockets to be exposed by the rack. The inputs
        can be patched with other modules via the bridge module \"INPUT\".</li>
        <li>:output-sockets The output sockets to be exposed by the rack. The outputs
        can be patched with other modules via the bridge module \"OUTPUT\".</li>
    </ul>
    <p>    
    The update function calls the update function of all modules. If the 
    rack has already been shut down the function immediately returns <b>nil</b>.
    Othwerwise it returns <b>t</b>.
    </p><p>
    The shutdown function calls the shutdown handlers of all modules and hooks. If the rack has 
    already been shut down the function immediately returns.
    </p>
    <p>See also: add-module</p>"
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (if (not environment)
      (signal-invalid-arguments-error
       :format-control "Environment must not be nil"
       :format-arguments nil))

  (let ((has-shut-down nil)
	(inputs nil) (outputs nil)
	(modules nil) ;; list of (:module module :name name)
	(hooks nil)
	(patches nil) ;; list of (:output-name "name" :output-socket <socket> :input-name "name" :input-socket <socket>)
	(compiled-rack nil))

    (labels ((get-module-name (module)
	       (let ((match
			 (find-if
			  (lambda (cur-module) (eq module (getf cur-module :module)))
			  modules)))
		 (if match (getf match :name) nil)))
	     (get-module-by-name (name)
	       (let ((module
		      (find-if
		       (lambda (m) (string= name (getf m :name)))
		       modules)))
		 (if module (getf module :module) nil)))
	     (add-module (module-name module)
	       (setf compiled-rack nil)
	       (push (list :module module :name module-name) modules))
	     (add-patch (output-name output-socket input-name input-socket)
			     (setf compiled-rack nil)
			     (push (list
				    :output-name output-name
				    :output-socket output-socket
				    :input-name input-name
				    :input-socket input-socket)
				   patches)))
      (flet ((compile-rack ()
	       (flet ((get-module-input-patches (module)
			"returns sparse list of (input-socket output-module output-socket)"
			(let* ((result nil)
			       (name (get-module-name module)))
			  (dolist (input-socket (funcall (getf module :inputs)))
			    (let ((patch
				   (find-if
				    (lambda (p)
				      (and
				       (string= (getf p :input-name) name)
				       (eq (getf p :input-socket) input-socket)))
				    patches)))
			      (if patch
				  (push (list
					 input-socket
					 (get-module-by-name (getf patch :output-name))
					 (getf patch :output-socket)) result)
				  (push (list input-socket nil nil) result))))
			  result)))
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
			     (funcall (getf h :update)))))))))))
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
		:modules (lambda() modules)
		:get-module-name (lambda (module) (get-module-name module))
		:get-module-by-name (lambda(name) (get-module-by-name name))
		:outputs (lambda() output-sockets)
		:inputs (lambda() input-sockets)
		:add-module (lambda (module-name module-fn &rest args)
			      (if (get-module-by-name module-name)
				  (signal-assembly-error
				   :format-control "A module with name ~a has already been added to the rack"
				   :format-arguments (list module-name)))
			      (let ((module (apply module-fn `(,module-name ,environment ,@args))))
				(dolist (property '(:inputs :outputs :update :get-output))
				  (if (not (functionp (getf module property)))
				      (signal-assembly-error
				       :format-control "Invalid module ~a: Property ~a must be a function but is ~a"
				       :format-arguments (list module-name property (getf module property)))))
				(add-module module-name module)))
		:add-hook (lambda (hook)
			    (setf compiled-rack nil)
			    (push hook hooks))
		:shutdown (lambda()
			    (if (not has-shut-down)
				(progn
				  (setf has-shut-down t)
				  (dolist (module modules)
				    (let ((f (getf (getf module :module) :shutdown)))
				      (if f (funcall f))))
				  (dolist (m hooks)
				    (let ((h (getf m :shutdown)))
				      (if h (funcall h)))))))
		:environment environment
		:is-rack t
		;;:add-patch (lambda (&key output-name output-socket input-name input-socket)
		;;(add-patch output-name output-socket input-name input-socket))
		:add-patch (lambda (output-name output-socket input-name input-socket)
				(let ((source-module (get-module-by-name output-name))
				      (destination-module (get-module-by-name input-name)))
				  (if (not source-module)
				      (signal-assembly-error
				       :format-control "Cannot find output module ~a"
				       :format-arguments (list output-name)))
				  (if (not destination-module)
				      (signal-assembly-error
				       :format-control "Cannot find input module ~a"
				       :format-arguments (list input-name)))
				  (if (not (find output-socket (funcall (getf source-module :outputs))))
				      (signal-assembly-error
				       :format-control "Module ~a does not expose output socket ~a"
				       :format-arguments (list output-name output-socket)))
				  (if (not (find input-socket (funcall (getf destination-module :inputs))))
				      (signal-assembly-error
				       :format-control "Module ~a does not expose input socket ~a"
				       :format-arguments (list input-name input-socket)))
				  (let ((p (find-if
					    (lambda (p)
					      (and (string= input-name (getf p :input-name))
						   (eq input-socket (getf p :input-socket))))
					    patches)))
				    (if p (signal-assembly-error
					   :format-control
					   "Input socket ~a of module ~a is already connected to output socket ~a of module ~a"
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
				    (if p (signal-assembly-error
					   :format-control
					   "Output socket ~a of module ~a is already connected to input socket ~a of module ~a"
					   :format-arguments (list
							      output-socket
							      output-name
							      (getf p :input-socket)
							      (getf p :input-name)))))
				  (add-patch output-name output-socket input-name input-socket)))
		:patches (lambda() patches))))
	  ;; Add bridge modules
	  ;;
	  (add-module "INPUT"
		      (list
		       :inputs (lambda() nil)
		       :outputs (lambda() input-sockets)
		       :update (lambda (args) (declare (ignore args)) nil)
		       :get-output (lambda(socket) (getf inputs socket))))
	  
	  (add-module "OUTPUT"
		      (list
		       :inputs (lambda() output-sockets)
		       :outputs (lambda() nil)
		       :update (lambda (args) (setf outputs args))
		       :get-output (lambda(socket) (getf outputs socket))))

	  rack)))))

;;
;; Helper functions / wrappers
;; (also for documentation purposes)
;;

(defun get-environment (rack)
  "Returns the environment of the rack."
  (getf rack :environment))

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
                   list is allocated during compilation of the rack and then used for all update calls
                   of the module.</li>
		<li>:get-output A function that is called in order to get the value of a specific
		    output socket. The function is called with a keyword that identifies the output socket
		    whose state is to be returned. The function must not modify the value
		    of the given or any other output socket.</li>
		<li>:shutdown An optional function with no arguments that is called when the rack
		    is shutting down.</li>
                <li>:get-state An optional function that can be used to expose internal states 
                    of the module, for example a VCO may expose its frequency. The function has one 
                    argument that consists of a keyword identifying the requested state, for 
                    example :frequency.</li>
	    </ul>
            <p>
	    A module must not add or remove input/output sockets after it has been instantiated.
            </p>
	</li>
	<li>&rest args Arbitrary additional arguments to be passed to the module instantiation function.
	    These arguments typically consist of keyword parameters.</li>
    </ul>
    Returns the added module."
  (apply (getf rack :add-module) module-name module-fn args))
  
(defun add-hook (rack hook)
  "Adds a hook to the rack. A hook is called each time after the rack has updated its state.
   A hook consists a property list with the following keys:
   <ul>
      <li>:update A function with no arguments that is called after the rack has updated its state.</li>
      <li>:shutdown A function with no arguments that is called when the rack is shutting down.</li>
   </ul>
   Hooks must not modify the rack. See also <b>cl-synthesizer-monitor:add-monitor</b>."
  (funcall (getf rack :add-hook) hook))

(defun get-module-name (rack module)
  "Get the name of a module. The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
	<li>module The module.</li>
    </ul>
   Returns the name or nil if the module does not belong to the rack"
  (funcall (getf rack :get-module-name) module))

(defun get-module (rack name)
  "Get a module of a rack. The function has the following arguments:
    <ul>
      <li>rack The rack.</li>
      <li>name The name of the module.</li>
    </ul>
   Returns the module or nil."
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

(defun add-patch (rack output-module-name output-socket input-module-name input-socket)
  "Adds a patch to the rack. A patch is an unidirectional connection between an output socket
    of a source module and an input socket of a destination module. The rack supports cycles 
    which means that an output socket of a module can be patched with one of its inputs (typically via
    multiple hops through other modules). The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
	<li>output-module-name Name of the output (source) module.</li>
	<li>output-socket A keyword representing one of the output sockets of the
	    output module.</li>
	<li>input-module-name Name of the input (destination) module.</li>
	<li>input-socket A keyword representing one of the input sockets of the
	    input module.</li>
    </ul>
    The rack signals an assembly-error in the following cases:
    <ul>
	<li>A module with the given output name does not exist.</li>
	<li>A module with the given input name does not exist.</li>
	<li>The given output-socket is already connected with a module.</li>
	<li>The given output-socket is not exposed by the output module.</li>
	<li>The given input-socket is already connected with a module.</li>
	<li>The given input-socket is not exposed by the input module.</li>
    </ul>"
  (funcall (getf rack :add-patch) output-module-name output-socket input-module-name input-socket))

(defun get-patches (rack)
  "Get all patches of a rack. The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
    </ul>
   Returns a list of property lists with the following keys:
   <ul>
     <li>:output-name Name of the output module.</li>
     <li>:output-socket Output socket. </li>
     <li>:input-name Name of the input module. </li>
     <li>:input-socket Input socket.</li>
   </ul>"
  (funcall (getf rack :patches)))

(defun get-modules (rack)
  "Get all modules of a rack. The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
    </ul>
    Returns a list of modules where each module consists of a property list with
    the following keys:
    <ul>
       <li>:module The module</li>
       <li>:name Name of the module</li>
    </ul>"
  (funcall (getf rack :modules)))

(defun is-rack (module)
  "Returns <b>t</b> if the given module represents a rack."
  (getf module :is-rack))

(defun play-rack (rack &key duration-seconds)
  "A utility function that \"plays\" the rack by consecutively calling its update function
    for a given number of \"ticks\". The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
	<li>:duration-seconds Duration in seconds of how long to play the rack. If for
	    example the duration is 2 seconds and the sample rate of the rack as declared
	    by its environment is 44100, then the update function of the rack will be called 88200 times.</li>
    </ul>"
  (let ((sample-rate (floor (getf (getf rack :environment) :sample-rate))) (update-fn (getf rack :update)))
    (dotimes (i (* duration-seconds sample-rate))
      (funcall update-fn nil)))
  (funcall (getf rack :shutdown))
  "DONE")


