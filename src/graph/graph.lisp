(in-package :cl-synthesizer-graph)



#|
(defclass graph () ())

(defgeneric add-vertex (graph id) (:documentation "Returns a vertex instance or throws"))

(defgeneric add-vertex-input (graph vertex input-id input-setter-fn))

(defgeneric add-vertex-output (graph vertex output-id output-getter-fn))

(defgeneric update-vertex (graph vertex))

|#


(defgeneric get-inputs-fn (module))

(defgeneric get-inputs (module))

(defgeneric get-outputs-fn (module))

(defgeneric get-outputs (module))

(defgeneric get-update-fn (module))

(defgeneric update (module))

(defgeneric get-state-fn (module))

(defgeneric get-state (module key))

(defgeneric shutdown (module))

(defgeneric get-modules (rack))
#|
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
|#

(defgeneric get-patches (rack))
#|
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
|#

(defgeneric is-rack (module))
#|
  "Returns <b>t</b> if the given module represents a rack."
  (getf module :is-rack))
|#


(defgeneric add-module (rack module-name module-fn &rest args))
#|
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
|#


(defgeneric get-module-name (rack module))
#|
  "Get the name of a module. <p>The function has the following parameters:
    <ul>
	<li>rack The rack.</li>
	<li>module The module.</li>
    </ul></p>
   Returns the name or nil if the module does not belong to the rack"
|#

(defgeneric get-module (rack name))
#|
  "Get a module of a rack. <p>The function has the following parameters:
    <ul>
      <li>rack The rack.</li>
      <li>name The name of the module.</li>
    </ul></p>
   Returns the module or nil."
|#

(defgeneric get-environment (rack))
#|
  "Returns the environment of the rack."
  (getf rack :environment))
|#

(defgeneric get-hooks (rack))

(defgeneric add-hook (rack hook))
#|
  "Adds a hook to the rack. A hook is called each time after the rack has updated its state.
   <p>A hook consists a property list with the following keys:
   <ul>
      <li>:update A function with no parameters that is called after the rack has updated its state.</li>
      <li>:shutdown A function with no parameters that is called when the rack is shutting down.</li>
   </ul></p>
   Hooks must not modify the rack. See also <b>cl-synthesizer-monitor:add-monitor</b>."
  (funcall (getf rack :add-hook) hook))
|#


(defgeneric add-patch (rack output-module-name output-socket input-module-name input-socket))
#|
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
  (funcall (getf rack :add-patch) output-module-name output-socket input-module-name input-socket))
|#



;;
;;
;;

(defgeneric expose-input-socket (rack rack-input-socket input-module-name input-socket))
#|
  "Exposes an input socket of a module as an input socket of the rack. <p>The function has the following parameters:
   <ul>
    <li>rack The rack.</li>
    <li>rack-input-socket A keyword representing the input socket of the rack.</li>
    <li>input-module-name Name of the module whose input is to be exposed.</li>
    <li>input-socket A keyword representing one of the inputs of the module.</li>
  </ul></p>"
  (funcall (getf rack :expose-input-socket) rack-input-socket input-module-name input-socket))
|#

(defgeneric expose-output-socket (rack rack-output-socket output-module-name output-socket))
#|
  "Exposes an output socket of a module as an output socket of the rack. <p>The function has the following parameters:
   <ul>
    <li>rack The rack.</li>
    <li>rack-output-socket A keyword representing the output socket of the rack.</li>
    <li>output-module-name Name of the module whose output is to be exposed.</li>
    <li>output-socket A keyword representing one of the outputs of the module.</li>
  </ul></p>"
  (funcall (getf rack :expose-output-socket) rack-output-socket output-module-name output-socket))
|#

;;
;; Patches
;;

(defgeneric make-patch (&key output-name output-socket input-name input-socket))
#|
  "TODO Create class. Do not expose via cl-synthesizer."
  (list
   :output-name output-name
   :output-socket output-socket
   :input-name input-name
   :input-socket input-socket))
|#

(defgeneric get-patch-output-name (patch))
(defgeneric get-patch-output-socket (patch))
(defgeneric get-patch-input-name (patch))
(defgeneric get-patch-input-socket (patch))

;;
;; Exposed sockets
;;

(defgeneric get-exposed-input-socket (rack socket))

(defgeneric get-exposed-output-socket (rack socket))
