(in-package :cl-synthesizer)

;;
;; Bridge Modules
;;
;; The inputs and outputs of a rack are represented
;; by bridge modules INPUT and OUTPUT
;;


(defun make-bridge-accessors (sockets)
  (let ((values (make-array (length sockets) :initial-element nil))
	(getters nil)
	(setters nil)
	(index 0))
    (dolist (socket sockets)
      (let ((cur-index index))
	;; setter plist
	(push (lambda (value) (setf (elt values cur-index) value)) setters)
	(push socket setters)
	;; getter plist
	(push (lambda () (elt values cur-index)) getters)
	(push socket getters))
      (setf index (+ 1 index)))
    (values getters setters)))

(defun make-input-bridge-module (input-sockets)
  "Exposes public output getters for patching. Exposes private input setters that are called by the rack." 
  (multiple-value-bind (getters setters)
      (make-bridge-accessors input-sockets)
    (list
     :inputs-private (lambda() setters)
     :inputs (lambda() nil) ;; no inputs that can be accessed via patching
     :outputs (lambda() getters)
     :update (lambda() nil))))

(defun make-output-bridge-module (output-sockets)
  "Exposes public input setters for patching. Exposes private output getters that are called by the rack."
  (multiple-value-bind (getters setters)
      (make-bridge-accessors output-sockets)
    (list
     :inputs (lambda() setters)
     :outputs-private (lambda() getters)
     :outputs (lambda() nil) ;; no outputs that can be accessed via patching
     :update (lambda () nil))))

;;
;; Rack
;;

(defun get-module-name (rack module)
  "Get the name of a module. The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
	<li>module The module.</li>
    </ul>
   Returns the name or nil if the module does not belong to the rack"
  (let ((match
	    (find-if
	     (lambda (cur-module) (eq module (getf cur-module :module)))
	     (funcall (getf rack :modules)))))
    (if match (getf match :name) nil)))

(defun get-module (rack name)
  "Get a module of a rack. The function has the following arguments:
    <ul>
      <li>rack The rack.</li>
      <li>name The name of the module.</li>
    </ul>
   Returns the module or nil."
  (let ((module
	 (find-if
	  (lambda (m) (string= name (getf m :name)))
	  (funcall (getf rack :modules)))))
    (if module (getf module :module) nil)))

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
		<li>:inputs A function with no arguments that returns a property list representing the
                    input sockets and their corresponding setter functions that are exposed by the module.</br>
                    Example: <code>:inputs (lambda() (list :input-1 (lambda(value) (setf input-1 value))))</code>
                    </br>Modules are supposed to buffer this list as the inputs might be requested several times.
                </li>
		<li>:outputs A function with no arguments that returns a property list representing 
                    the  output sockets and their corresponding getter functions that exposed by the module.</br>
                    Example: <code>:outputs (lambda() (list :output-1 (lambda() output-1)))</code>
                    </br>Modules are supposed to buffer this list as the outputs might be requested several times.
                </li>
		<li>:update A function with no arguments that updates the outputs according to the previously set inputs.</li>
		<li>:shutdown An optional function with no arguments that is called when the rack
		    is shutting down.</li>
                <li>:get-state An optional function that can be used to expose internal states 
                    of the module, for example a VCO may expose its frequency. The function has one 
                    argument that consists of a keyword identifying the requested state, for 
                    example :frequency.</li>
	    </ul>
	</li>
	<li>&rest args Arbitrary additional arguments to be passed to the module instantiation function.
	    These arguments typically consist of keyword parameters.</li>
    </ul>
    Returns the module."
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
      (let* ((module (get-module rack (first module-path))))
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
      (funcall update-fn)))
  (funcall (getf rack :shutdown))
  "DONE")

;;
;; The rack
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

  (let ((this nil)
	(has-shut-down nil)
	;; list of (:module module :name name)
	(modules nil)
	;; List of lambda()
	(hooks nil)
	;; list of (:output-name "name" :output-socket <socket> :input-name "name" :input-socket <socket>)
	(patches nil)
	(compiled-rack nil)
	(input-bridge-module (make-input-bridge-module input-sockets))
	(output-bridge-module (make-output-bridge-module output-sockets)))
    
    (labels ((add-module (module-name module)
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
      (let ((rack
	     (list
	      :modules (lambda() modules)
	      ;; delegate to bridge module
	      :outputs (getf output-bridge-module :outputs-private)
	      ;; delegate to bridge module
	      :inputs (getf input-bridge-module :inputs-private)
	      :patches (lambda() patches)
	      :hooks (lambda () hooks)
	      :update (lambda ()
			(if has-shut-down
			    nil
			    (progn
			      (if (not compiled-rack)
				  (setf compiled-rack (cl-synthesizer-rack-compiler:compile-rack this)))
			      (funcall compiled-rack)
			      t)))
	      :add-module (lambda (module-name module-fn &rest args)
			    (if (get-module this module-name)
				(signal-assembly-error
				 :format-control "A module with name ~a has already been added to the rack"
				 :format-arguments (list module-name)))
			    (let ((module (apply module-fn `(,module-name ,environment ,@args))))
			      (dolist (property '(:inputs :outputs :update))
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
	      :add-patch (lambda (output-name output-socket input-name input-socket)
			   (let ((source-module (get-module this output-name))
				 (destination-module (get-module this input-name)))
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
			     (add-patch output-name output-socket input-name input-socket))))))

	(setf this rack)
	;;
	;; Add bridge modules
	;;
	(add-module "INPUT" input-bridge-module)
	(add-module "OUTPUT" output-bridge-module)

	rack))))

