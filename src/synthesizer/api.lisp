;;
;; Api of cl-synthesizer
;;

(in-package :cl-synthesizer)

;;
;; API
;;

(defun get-module-name (rack module)
  "Get the name of a module. <p>The function has the following parameters:
    <ul>
	<li>rack The rack.</li>
	<li>module The module.</li>
    </ul></p>
   Returns the name or nil if the module does not belong to the rack"
  (cl-synthesizer-graph:get-vertex-name rack module))

(defun get-module (rack name)
  "Get a module of a rack. <p>The function has the following parameters:
    <ul>
      <li>rack The rack.</li>
      <li>name The name of the module.</li>
    </ul></p>
   Returns the module or nil."
  (let ((module
	 (find-if
	  (lambda (m) (string= name (getf m :name)))
	  (funcall (getf rack :modules)))))
    (if module (getf module :module) nil)))
  

(defun get-inputs-fn (module)
  (cl-synthesizer-graph:get-inputs-fn module))

(defun get-inputs (module)
  (cl-synthesizer-graph:get-inputs module))

(defun get-outputs-fn (module)
  (cl-synthesizer-graph:get-outputs-fn module))

(defun get-outputs (module)
  (cl-synthesizer-graph:get-outputs module))

(defun get-update-fn (module)
  (cl-synthesizer-graph:get-update-fn module))

(defun update (module)
  (cl-synthesizer-graph:update module))

(defun get-state-fn (module)
  (cl-synthesizer-graph:get-state-fn module))

(defun get-state (module key)
  (cl-synthesizer-graph:get-state module key))

(defun shutdown (module)
  (cl-synthesizer-graph:shutdown module))

(defun get-modules (rack)
  "Get all modules of a rack. <p>The function has the following parameters:
    <ul>
	<li>rack The rack.</li>
    </ul></p>
    Returns a list of modules where each module consists of a property list with
    the following keys:
    <ul>
       <li>:module The module</li>
       <li>:name Name of the module</li>
    </ul>"
  (let ((module-names nil))
    (cl-synthesizer-graph:get-vertices
     rack
     (lambda (module-name)
       (push module-name module-names)))
    (mapcar
     (lambda (module-name)
       (list
	:name module-name
	:module (get-module rack module-name)))
     module-names)))

;;
;; Patch stuff
;;

(defun get-patch-output-name (edge)
  (getf edge :output-name))

(defun get-patch-output-socket (edge)
  (getf edge :output-socket))

(defun get-patch-input-name (edge)
  (getf edge :input-name))

(defun get-patch-input-socket (edge)
  (getf edge :input-socket))

(defun get-patches (rack)
  "Get all patches of a rack. <p>The function has the following parameters:
    <ul>
	<li>rack The rack.</li>
    </ul></p>
   Returns a list of property lists with the following keys:
   <ul>
     <li>:output-name Name of the output module.</li>
     <li>:output-socket Output socket. </li>
     <li>:input-name Name of the input module. </li>
     <li>:input-socket Input socket.</li>
   </ul>"
  (let ((patches nil))
    (cl-synthesizer-graph:get-edges
     rack
     (lambda (output-vertex-name output-socket input-vertex-name input-socket)
       (push
	(list
	 :output-name output-vertex-name
	 :output-socket output-socket
	 :input-name input-vertex-name
	 :input-socket input-socket)
	patches)))
    patches))

;;
;;
;;

(defun is-rack (module)
  "Returns <b>t</b> if the given module represents a rack."
  (cl-synthesizer-graph:is-graph module))

(defun add-module (rack module-name module-fn &rest args)
  "Adds a module to a rack. <p>The function has the following parameters:
    <ul>
	<li>rack The rack.</li>
	<li>module-name Unique name of the module, for example \"VCO-1\". If the name
	    is already used by another module an assembly-error is signalled.</li>
	<li>module-fn A function that instantiates the module. This function is
	    called by the rack with the following parameters:
	    <ul>
		<li>name Name of the module.</li>
		<li>environment The synthesizer environment.</li>
		<li>module-args Any additional arguments passed to add-module.</li>
	    </ul>
	    The module instantiation function must return a property list with the following keys:
	    <ul>
		<li>:inputs A function with no parameters that returns a property list representing the
                    input sockets and their corresponding setter functions that are exposed by the module.</br>
                    Example: <code>:inputs (lambda() (list :input-1 (lambda(value) (setf input-1 value))))</code>
                    </br>Modules are supposed to buffer this list as the inputs might be requested several times.
                </li>
		<li>:outputs A function with no parameters that returns a property list representing 
                    the  output sockets and their corresponding getter functions that exposed by the module.</br>
                    Example: <code>:outputs (lambda() (list :output-1 (lambda() output-1)))</code>
                    </br>Modules are supposed to buffer this list as the outputs might be requested several times.
                </li>
		<li>:update A function with no parameters that updates the outputs according to the previously set inputs.</li>
		<li>:shutdown An optional function with no parameters that is called when the rack
		    is shutting down.</li>
                <li>:state An optional function that can be used to expose internal states 
                    of the module, for example a VCO may expose its frequency. The function has one 
                    parameter that consists of a keyword identifying the requested state, for 
                    example :frequency.</li>
	    </ul>
	</li>
	<li>&rest args Arbitrary additional arguments to be passed to the module instantiation function.
	    These arguments typically consist of keyword parameters.</li>
    </ul></p>
    Returns the module."
  (apply #'cl-synthesizer-graph:add-vertex rack module-name module-fn args))

(defun get-environment (rack)
  "Returns the environment of the rack."
  (cl-synthesizer-graph:get-environment rack))

(defun get-hooks (rack)
  (cl-synthesizer-graph:get-hooks rack))

(defun add-hook (rack hook)
  "Adds a hook to the rack. A hook is called each time after the rack has updated its state.
   <p>A hook consists a property list with the following keys:
   <ul>
      <li>:update A function with no parameters that is called after the rack has updated its state.</li>
      <li>:shutdown A function with no parameters that is called when the rack is shutting down.</li>
   </ul></p>
   Hooks must not modify the rack. See also <b>cl-synthesizer-monitor:add-monitor</b>."
  (cl-synthesizer-graph:add-hook rack hook))

(defun add-patch (rack output-module-name output-socket input-module-name input-socket)
  "Adds a patch to the rack. A patch is an unidirectional connection between an output socket
    of a source module and an input socket of a destination module. The rack supports cycles 
    which means that an output socket of a module can be patched with one of its inputs (typically via
    multiple hops through other modules). <p>The function has the following parameters:
    <ul>
	<li>rack The rack.</li>
	<li>output-module-name Name of the output (source) module.</li>
	<li>output-socket A keyword representing one of the output sockets of the
	    output module.</li>
	<li>input-module-name Name of the input (destination) module.</li>
	<li>input-socket A keyword representing one of the input sockets of the
	    input module.</li>
    </ul></p>
    <p>The rack signals an assembly-error in the following cases:
    <ul>
	<li>A module with the given output name does not exist.</li>
	<li>A module with the given input name does not exist.</li>
	<li>The given output-socket is already connected with a module.</li>
	<li>The given output-socket is not exposed by the output module.</li>
	<li>The given input-socket is already connected with a module.</li>
	<li>The given input-socket is not exposed by the input module.</li>
    </ul></p>"
  (cl-synthesizer-graph:add-edge
   rack
   output-module-name
   output-socket
   input-module-name
   input-socket))

;;
;;
;;

(defun expose-input-socket (rack rack-input-socket input-module-name input-socket)
  "Exposes an input socket of a module as an input socket of the rack. <p>The function has the following parameters:
   <ul>
    <li>rack The rack.</li>
    <li>rack-input-socket A keyword representing the input socket of the rack.</li>
    <li>input-module-name Name of the module whose input is to be exposed.</li>
    <li>input-socket A keyword representing one of the inputs of the module.</li>
  </ul></p>"
  (cl-synthesizer-graph:expose-input-socket
   rack
   rack-input-socket
   input-module-name
   input-socket))

(defun expose-output-socket (rack rack-output-socket output-module-name output-socket)
  "Exposes an output socket of a module as an output socket of the rack. <p>The function has the following parameters:
   <ul>
    <li>rack The rack.</li>
    <li>rack-output-socket A keyword representing the output socket of the rack.</li>
    <li>output-module-name Name of the module whose output is to be exposed.</li>
    <li>output-socket A keyword representing one of the outputs of the module.</li>
  </ul></p>"
  (cl-synthesizer-graph:expose-output-socket
   rack
   rack-output-socket
   output-module-name
   output-socket))

;;
;; Exposed sockets
;;

(defun get-exposed-input-socket (rack socket)
  (cl-synthesizer-graph:get-exposed-input-socket rack socket))

(defun get-exposed-output-socket (rack socket)
  (cl-synthesizer-graph:get-exposed-output-socket rack socket))

;;
;; Graph Impl
;;

(defmethod cl-synthesizer-graph:get-inputs-fn (vertex)
  (getf vertex :inputs))

(defmethod cl-synthesizer-graph:get-inputs (vertex)
  (funcall (getf vertex :inputs)))

(defmethod cl-synthesizer-graph:get-outputs-fn (vertex)
  (getf vertex :outputs))

(defmethod cl-synthesizer-graph:get-outputs (vertex)
  (funcall (getf vertex :outputs)))

(defmethod cl-synthesizer-graph:get-update-fn (vertex)
  (getf vertex :update))

(defmethod cl-synthesizer-graph:update (vertex)
  (funcall (getf vertex :update)))

(defmethod cl-synthesizer-graph:get-state-fn (vertex)
  (getf vertex :state))

(defmethod cl-synthesizer-graph:get-state (vertex key)
  (let ((fn (cl-synthesizer-graph:get-state-fn vertex)))
    (if fn
	(funcall fn key)
	nil)))

(defmethod cl-synthesizer-graph:shutdown (vertex)
  (let ((fn (getf vertex :shutdown)))
    (if fn (funcall fn))))

(defmethod cl-synthesizer-graph:get-vertices (graph callback-fn)
  (dolist (module (funcall (getf graph :modules)))
    (funcall
     callback-fn
     (getf module :name)))) 

(defmethod cl-synthesizer-graph:get-edges (graph edge-callback-fn)
  (let ((patches (funcall (getf graph :patches))))
    (dolist (patch patches)
      (funcall
       edge-callback-fn
       (getf patch :output-name)
       (getf patch :output-socket)
       (getf patch :input-name)
       (getf patch :input-socket)))))

(defmethod cl-synthesizer-graph:is-graph (vertex)
  (getf vertex :is-rack))

(defmethod cl-synthesizer-graph:add-vertex
    (graph
     vertex-name vertex-ctor-fn
     &rest args)
  (apply (getf graph :add-module) vertex-name vertex-ctor-fn args))

(defmethod cl-synthesizer-graph:get-vertex-name (graph vertex)
  (let ((match
	    (find-if
	     (lambda (cur-module) (eq vertex (getf cur-module :module)))
	     (cl-synthesizer:get-modules graph))))
    (if match (getf match :name) nil)))

(defmethod cl-synthesizer-graph:get-vertex (graph vertex-name)
  (let ((module
	 (find-if
	  (lambda (m) (string= vertex-name (getf m :name)))
	  (cl-synthesizer:get-modules graph))))
    (if module (getf module :module) nil)))

(defmethod cl-synthesizer-graph:get-environment (graph)
  (getf graph :environment))

(defmethod cl-synthesizer-graph:get-hooks (graph)
  (funcall (getf graph :hooks)))

(defmethod cl-synthesizer-graph:add-hook (graph hook)
  (funcall (getf graph :add-hook) hook))

(defmethod cl-synthesizer-graph:add-edge
    (graph
     output-vertex-name
     output-socket
     input-vertex-name
     input-socket)
  (funcall
   (getf graph :add-patch)
   output-vertex-name
   output-socket
   input-vertex-name
   input-socket))

(defmethod cl-synthesizer-graph:expose-input-socket
    (graph
     graph-input-socket
     input-vertex-name
     input-socket)
  (funcall
   (getf graph :expose-input-socket)
   graph-input-socket
   input-vertex-name
   input-socket))

(defmethod cl-synthesizer-graph:expose-output-socket
    (graph
     graph-output-socket
     output-vertex-name
     output-socket)
  (funcall
   (getf graph :expose-output-socket)
   graph-output-socket
   output-vertex-name
   output-socket))

(defmethod cl-synthesizer-graph:get-exposed-input-socket (graph socket)
  (funcall (getf graph :get-exposed-input-socket) socket))

(defmethod cl-synthesizer-graph:get-exposed-output-socket (graph socket)
  (funcall (getf graph :get-exposed-output-socket) socket))


