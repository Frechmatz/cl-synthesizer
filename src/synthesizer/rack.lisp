(in-package :cl-synthesizer)

;;
;; Rack-Module
;; 

(defclass rack-module ()
  ((state :initarg nil :documentation ":PROCESS-TICK, :PROCESSING-TICK, :TICK-PROCESSED")
   (name :initarg nil)
   (module :initarg nil)
   (module-input-sockets :initarg nil)
   (module-output-sockets :initarg nil)
   (input-patches :initform (make-hash-table))
   (output-patches :initform (make-hash-table))
   (input-argument-list-prototype :initform nil))
  (:documentation "Represents a module holding input/output connections to other modules"))

(defmethod initialize-instance :after ((rm rack-module) &key name module)
  (setf (slot-value rm 'name) name)
  (setf (slot-value rm 'module) module)
  (setf (slot-value rm 'module-input-sockets) 
	(funcall (getf (slot-value rm 'module) :inputs)))
  (setf (slot-value rm 'module-output-sockets)
	(funcall (getf (slot-value rm 'module) :outputs)))
  ;; Prepare prototype of parameter with which
  ;; the update function of the module will be called.
  ;; (:INPUT-1 nil :INPUT-2 nil ...)
  (dolist (input-socket (slot-value rm 'module-input-sockets))
      (push nil (slot-value rm 'input-argument-list-prototype))
      (push input-socket (slot-value rm 'input-argument-list-prototype))))

(declaim (inline get-rack-module-input-argument-list-prototype))
(defun get-rack-module-input-argument-list-prototype (rm)
  ;;(copy-list (slot-value rm 'input-argument-list-prototype)))
  (slot-value rm 'input-argument-list-prototype))

(declaim (inline get-rack-module-input-sockets))
(defun get-rack-module-input-sockets (rm)
  (slot-value rm 'module-input-sockets))

(declaim (inline get-rack-module-output-sockets))
(defun get-rack-module-output-sockets (rm)
  (slot-value rm 'module-output-sockets))

(defun get-rack-module-name (rm)
  (slot-value rm 'name))

(declaim (inline get-rack-module-state))
(defun get-rack-module-state (rm)
  (slot-value rm 'state))

(declaim (inline set-rack-module-state))
(defun set-rack-module-state (rm state)
  (setf (slot-value rm 'state) state))

(declaim (inline get-rack-module-update-fn))
(defun get-rack-module-update-fn (rm)
  (getf (slot-value rm 'module) :update))

(defun get-rack-module-module (rm)
  (slot-value rm 'module))

(declaim (inline get-rack-module-output-fn))
(defun get-rack-module-output-fn (rm)
  (getf (slot-value rm 'module) :get-output))

(defun get-rack-module-output (rm socket)
  (funcall (getf (slot-value rm 'module) :get-output) socket))

(defun get-rack-module-shutdown-fn (rm)
  (let ((f (getf (slot-value rm 'module) :shutdown)))
    (if f f (lambda() ()))))

(declaim (inline get-rack-module-input-patch))
(defun get-rack-module-input-patch (rm input-socket)
  (gethash input-socket (slot-value rm 'input-patches)))

(defun add-rack-module-input-patch (rm input-socket patch)
  ;;(setf (gethash destination-input-socket (slot-value destination-rm 'input-patches))
  ;; (make-rack-module-patch source-rm source-output-socket))
  (setf (gethash input-socket (slot-value rm 'input-patches)) patch))

(defun add-rack-module-output-patch (rm output-socket patch)
  ;;(setf (gethash source-output-socket (slot-value source-rm 'output-patches))
  ;;	  (make-rack-module-patch destination-rm destination-input-socket))))
  (setf (gethash output-socket (slot-value rm 'output-patches)) patch))


(defun get-rack-module-output-patch (rm output-socket)
  (gethash output-socket (slot-value rm 'output-patches)))


;;
;; Patch
;;

(defclass rack-module-patch ()
  ((rack-module :initarg nil)
   (socket :initarg nil))
  (:documentation "Represents an end-point to which an input/output socket of a module is connected."))

(defun get-rack-patch-target-name (patch)
  (get-rack-module-name (slot-value patch 'rack-module)))

(declaim (inline get-rack-patch-socket))
(defun get-rack-patch-socket (patch)
  (slot-value patch 'socket))

(declaim (inline get-rack-patch-module))
(defun get-rack-patch-module (patch)
  (slot-value patch 'rack-module))

(defun make-rack-module-patch (rm socket)
  (let ((c (make-instance 'rack-module-patch)))
    (setf (slot-value c 'rack-module) rm)
    (setf (slot-value c 'socket) socket)
    c))

;;
;;
;; Rack
;;
;;

(defun get-environment (rack)
  "Returns the environment of the rack."
  (getf rack :environment))

(declaim (inline set-state))
(defun set-state (rack state)
  (dolist (m (getf rack :modules))
    (setf (slot-value m 'state) state)))

(defun get-rm-module (rack name)
  "Helper function that returns internal representation of a module or nil."
  (find-if (lambda (rm) (string= name (get-rack-module-name rm))) (getf rack :modules)))



(defun get-module (rack name)
  "Get a module of a rack. The function has the following arguments:
    <ul>
      <li>rack The rack.</li>
      <li>name The name of the module</li>
    </ul>
   Returns the module (represented as a property list) or nil if a module
   with the given name has not been added to the rack."
  (let ((rm (find-if (lambda (rm) (string= name (get-rack-module-name rm))) (getf rack :modules))))
    (if rm
	(slot-value rm 'module)
	nil)))

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
      (let* ((rm (find-if (lambda (rm) (string= (first module-path) (get-rack-module-name rm))) (getf rack :modules))))
	(if rm
	    (let ((module (slot-value rm 'module)))
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
  (push hook (getf rack :hooks)))

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
  (if (get-rm-module rack module-name)
      (signal-assembly-error
       :format-control "A module with name ~a has already been added to the rack"
       :format-arguments (list module-name)))

  (let ((environment (getf rack :environment)))
    (let ((rm (make-instance 'rack-module
			     :module (apply module-fn `(,module-name ,environment ,@args))
			     :name module-name)))
      (let ((m (get-rack-module-module rm)))
	(dolist (property '(:inputs :outputs :update :get-output))
	  (if (not (functionp (getf m property)))
	      (signal-assembly-error
	       :format-control "Invalid module ~a: Property ~a must be a function but is ~a"
	       :format-arguments (list module-name property (getf m property))))))

      (push rm (getf rack :modules))
      nil)))

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

  (let* ((this nil) (has-shut-down nil) (input-rm nil)
	 (inputs nil) (output-rm nil) (outputs nil))
    (let ((rack
	   (list
	    :modules nil
	    :hooks nil
	    :outputs (lambda() output-sockets)
	    :inputs (lambda() input-sockets)
	    :update
	    (lambda (args)
	      ;;(declare (optimize (debug 3) (speed 0) (space 0)))
	      (declare (inline set-rack-module-state
			       get-rack-module-state
			       get-rack-module-input-sockets
			       get-rack-module-input-argument-list-prototype
			       get-rack-module-update-fn
			       get-rack-module-input-patch
			       get-rack-patch-module
			       get-rack-module-output-fn
			       get-rack-patch-socket
			       set-state))
	      (if has-shut-down
		  nil
		  (progn
		    (set-state this :PROCESS-TICK)
		    (setf inputs args)
		    (set-rack-module-state input-rm :PROCESSED-TICK)
		    (labels
			((update-rm (rm)
			   ;; Update a module
			   ;; If module is already updating do nothing
			   ;; Otherwise update all input modules and then update outputs
			   (let ((state (get-rack-module-state rm)))
			     (if (not (eq state :PROCESS-TICK))
				 nil ;; module is already processing -> do nothing
				 (progn
				   (set-rack-module-state rm :PROCESSING-TICK)
				   ;; update input modules
				   (dolist (cur-input-socket (get-rack-module-input-sockets rm))
				     (let ((patch (get-rack-module-input-patch rm cur-input-socket)))
				       (if patch 
					   (update-rm (get-rack-patch-module patch)))))
				   ;; update this
				   (let ((input-args (get-rack-module-input-argument-list-prototype rm)))
				     ;; collect inputs
				     (dolist (cur-input-socket (get-rack-module-input-sockets rm))
				       (let ((patch (get-rack-module-input-patch rm cur-input-socket))
					     (socket-input-value nil))
					 (if patch
					     (let* ((source-rm (get-rack-patch-module patch))
						    (source-rm-socket (get-rack-patch-socket patch))
						    (output-fn (get-rack-module-output-fn source-rm)))
					       (setf socket-input-value (funcall output-fn source-rm-socket))))
					 (setf (getf input-args cur-input-socket) socket-input-value)))
				     ;; call update function on this
				     (funcall (get-rack-module-update-fn rm) input-args)
				     (set-rack-module-state rm :PROCESSED-TICK)
				     ))))))
		      ;; for all modules
		      (dolist (rm (getf this :modules))
			(update-rm rm))
		      ;; for all hooks
		      (dolist (m (getf this :hooks))
			(funcall (getf m :update))))
		    t)))
	    :get-output
	    (lambda (socket)
	      (getf outputs socket))
	    :shutdown
	    (lambda()
	      (if (not has-shut-down)
		  (progn
		    (setf has-shut-down t)
		    (dolist (rm (getf this :modules))
		      (funcall (get-rack-module-shutdown-fn rm)))
		    (dolist (m (getf this :hooks))
		      (if (getf m :shutdown)
			  (funcall (getf m :shutdown)))))))
	    :environment environment
	    :is-rack t)))

      (setf this rack)

      (add-module rack "INPUT"
		  (lambda(name environment)
		    (declare (ignore name environment))
		    (list
		     :inputs (lambda() nil)
		     :outputs (lambda() input-sockets)
		     :update (lambda (args) (declare (ignore args)) nil)
		     :get-output (lambda(socket) (getf inputs socket)))))
      (setf input-rm (get-rm-module rack "INPUT"))
      
      (add-module rack "OUTPUT"
		  (lambda(name environment)
		    (declare (ignore name environment))
		    (list
		     :inputs (lambda() output-sockets)
		     :outputs (lambda() nil)
		     :update (lambda (args) (setf outputs args))
		     :get-output (lambda(socket) (getf outputs socket)))))
      (setf output-rm (get-rm-module rack "OUTPUT"))

      rack)))

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
  (let ((source-rm (get-rm-module rack source-rm-name))
	(destination-rm (get-rm-module rack destination-rm-name)))
    (if (not source-rm)
	(signal-assembly-error
	 :format-control "Cannot find source module ~a"
	 :format-arguments (list source-rm-name)))
    (if (not destination-rm)
	(signal-assembly-error
	 :format-control "Cannot find destination module ~a"
	 :format-arguments (list destination-rm-name)))
    (if (not (find source-output-socket (get-rack-module-output-sockets source-rm)))
	(signal-assembly-error
	 :format-control "Module ~a does not have output socket ~a"
	 :format-arguments (list (get-rack-module-name source-rm) source-output-socket)))
    (if (not (find destination-input-socket (get-rack-module-input-sockets destination-rm)))
	(signal-assembly-error
	 :format-control "Module ~a does not have input socket ~a"
	 :format-arguments (list (get-rack-module-name destination-rm) destination-input-socket)))
    (let ((p (get-rack-module-input-patch destination-rm destination-input-socket)))
      (if p
	  (signal-assembly-error
	   :format-control "Input socket ~a of module ~a is already connected to output socket ~a of module ~a"
	   :format-arguments (list
			      destination-input-socket
			      (get-rack-module-name destination-rm)
			      (get-rack-patch-target-name p)
			      (get-rack-patch-socket p)))))
    
    (let ((p (get-rack-module-output-patch source-rm source-output-socket)))
      (if p
	  (signal-assembly-error
	   :format-control "Output socket ~a of module ~a is already connected to input socket ~a of module ~a"
	   :format-arguments (list
			      source-output-socket
			      (get-rack-module-name source-rm)
			      (get-rack-patch-socket p)
			      (get-rack-patch-target-name p)))))

    ;;(setf (gethash destination-input-socket (slot-value destination-rm 'input-patches))
    ;;	  (make-rack-module-patch source-rm source-output-socket))
    (add-rack-module-input-patch
     destination-rm destination-input-socket
     (make-rack-module-patch source-rm source-output-socket))
    
    ;;(setf (gethash source-output-socket (slot-value source-rm 'output-patches))
    ;;  (make-rack-module-patch destination-rm destination-input-socket))))
    (add-rack-module-output-patch
     source-rm source-output-socket 
     (make-rack-module-patch destination-rm destination-input-socket))))

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
  (let ((rm (get-rm-module rack module-name)))
    (if (not rm)
	(values nil nil nil)
	(let ((patch nil))
	  (if (eq :input-socket socket-type)
	      (setf patch (get-rack-module-input-patch rm socket))
	      (setf patch (get-rack-module-output-patch rm socket)))
	  (if (not patch)
	      (values nil nil nil)
	      (values
	       (get-rack-patch-target-name patch)
	       (get-rack-module-module (get-rack-patch-module patch))
	       (get-rack-patch-socket patch)))))))


