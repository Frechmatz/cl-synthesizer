(in-package :cl-synthesizer)


(defun update (module)
  "Calls the update function of a module."
  (funcall (getf module :update)))

(defun shutdown (module)
  "Calls the (optional) shutdown function of a module."
  (let ((fn (getf module :shutdown)))
    (if fn (funcall fn))))

(defun get-modules (rack)
  "Returns the modules of a rack."
  (funcall (getf rack :modules)))

(defun get-patches (rack)
  "Returns a list of the patches of a rack. Each patch is represented by a property list with the following keys:
   <ul>
     <li>:output-name Name of the output module.</li>
     <li>:output-socket Output socket. </li>
     <li>:input-name Name of the input module. </li>
     <li>:input-socket Input socket.</li>
   </ul>"
  (funcall (getf rack :patches)))

(defun is-rack (module)
  "Returns t if the given module represents a rack."
  (getf module :is-rack))

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
		<li>:inputs A function with no parameters that returns a property list defining the
                    input sockets of the module. An input socket consists of a keyword and a property list with the following keys:
                    <ul>
                    <li>:set A function with one parameter to set the input value.</li>
                    <li>:get A function with no parameters to get the current input value.</li>
                    </ul>
                </li>
		<li>:outputs A function with no parameters that returns a property list defining
                    the output sockets of the module. An output socket consists of a keyword and a property list with the following keys:
                    <ul>
                    <li>:get A function with no parameters to get the current output value.</li>
                    </ul>
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
  (apply (getf rack :add-module) module-name module-fn args))

(defun get-module-name (module)
  "Returns the name of a module."
  (funcall (getf module :cl-synthesizer-module-get-name)))

(defun get-module-rack (module)
  "Returns the rack to which a module belongs."
  (funcall (getf module :cl-synthesizer-module-get-rack)))

(defun get-module (rack path)
  "Get a module by its name. <p>The function has the following parameters:
    <ul>
      <li>rack The rack.</li>
      <li>path The path of the module.</br>
         Example: \"VCO\"</br> 
         Example: '(\"VOICE-1\" \"VCO\")</li>
    </ul></p>
   Returns the module or nil."
  (funcall (getf rack :get-module) path))

(defun get-environment (rack)
  "Returns the environment of the rack."
  (getf rack :environment))

(defun get-hooks (rack)
  "Returns the hooks of a rack."
  (funcall (getf rack :hooks)))

(defun add-hook (rack hook)
  "Adds a hook to a rack. <p>The function has the following parameters:
   <ul>
      <li>rack The rack.</li>
      <li>hook The hook. A hook consists a property list with the following keys:
         <ul>
           <li>:updating An optional function with no parameters that is called before the rack is updated. This function can be used to set rack inputs.</li>
           <li>:updated An optional function with no parameters that is called after the rack has been updated.</li>
          <li>:shutdown An optional function with no parameters that is called when the rack is shutting down.</li></ul>
   </ul></p>
   Hooks must not add/remove modules or patches."
  (funcall (getf rack :add-hook) hook))

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
  (funcall (getf rack :add-patch) output-module-name output-socket input-module-name input-socket))

(defun add-rack-input (rack rack-input-socket input-module-name input-socket)
  "Exposes an input socket of a module as an input socket of the rack. <p>The function has the following parameters:
   <ul>
    <li>rack The rack.</li>
    <li>rack-input-socket A keyword representing the input socket of the rack.</li>
    <li>input-module-name Name of the module whose input is to be exposed.</li>
    <li>input-socket A keyword representing one of the inputs of the module.</li>
  </ul></p>"
  (funcall (getf rack :add-rack-input) rack-input-socket input-module-name input-socket))

(defun add-rack-output (rack rack-output-socket output-module-name output-socket)
  "Exposes an output socket of a module as an output socket of the rack. <p>The function has the following parameters:
   <ul>
    <li>rack The rack.</li>
    <li>rack-output-socket A keyword representing the output socket of the rack.</li>
    <li>output-module-name Name of the module whose output is to be exposed.</li>
    <li>output-socket A keyword representing one of the outputs of the module.</li>
  </ul></p>"
  (funcall (getf rack :add-rack-output) rack-output-socket output-module-name output-socket))

(defun play-rack (rack &key duration-seconds)
  "A utility function that \"plays\" the rack by consecutively calling its update function
    for a given number of \"ticks\". <p>The function has the following parameters:
    <ul>
	<li>rack The rack.</li>
	<li>:duration-seconds Duration in seconds of how long to play the rack. If for
	    example the duration is 2 seconds and the sample rate of the rack as declared
	    by its environment is 44100, then the update function of the rack will be called 88200 times.</li>
    </ul></p>"
  (let ((sample-rate (floor (getf (get-environment rack) :sample-rate)))
	(update-fn (getf rack :update)))
    (dotimes (i (floor (* duration-seconds sample-rate)))
      (funcall update-fn)))
  (cl-synthesizer:shutdown rack)
  "DONE")

