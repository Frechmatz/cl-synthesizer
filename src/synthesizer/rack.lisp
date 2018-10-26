(in-package :cl-synthesizer)

;;
;;
;; Rack
;;
;;

(defun get-environment (rack)
  "Returns the environment of the rack."
  (getf rack :environment))

(defun set-state (rack state)
  (dolist (m (getf rack :modules))
    (setf (slot-value m 'state) state)))

(defun attach-audio-device (rack device-ctor)
  "Attaches an audio output device to the rack."
  (add-module rack "SPEAKER" device-ctor)
  ;; TODO for now not a loop but just two channels
  (add-patch rack "LINE-OUT" :channel-1 "SPEAKER" :channel-1)
  (add-patch rack "LINE-OUT" :channel-2 "SPEAKER" :channel-2))
  
(defun attach-midi-in-device (rack device-ctor)
  "Attaches a MIDI input device to the rack. The rack currently supports one MIDI input device."
  (add-module rack "MIDI" device-ctor)
  (add-patch rack "MIDI" :midi-events "MIDI-IN" :midi-events))

(defun add-hook (rack hook)
  "Adds a hook to the rack. A hook is called each time after the rack has updated its state.
   A hook consists a property list with the following keys:
   <ul>
      <li>:update A function with no arguments that is called after the rack has updated its state.</li>
      <li>:shutdown A function with no arguments that is called when the rack is shutting down.</li>
   </ul>
   Hooks must not modify the rack. See also <b>cl-synthesizer-monitor:add-monitor</b>."
  (push hook (getf rack :hooks)))


(defun buffer (name environment &key sockets)
  (declare (ignore name environment))
  (let ((outputs (make-hash-table :test #'eq)))
    (list
     :inputs (lambda() sockets)
     :outputs (lambda() sockets)
     :get-output (lambda (output)
		   (gethash output outputs))
     :update (lambda (&rest args)
	       (dolist (socket sockets)
		 (setf (gethash socket outputs) (getf args socket)))))))

(defun make-rack (&key environment (additional-input-sockets nil) (additional-output-sockets))
  "Creates a rack. The function has the following arguments:
    <ul>
	<li>:environment The synthesizer environment.</li>
    </ul>
    A rack is initialized with the virtual modules \"LINE-OUT\" and 
    \"MIDI-IN\" that represent the interface to so called devices.
    A device is a system specific implementation that provides audio
    output or integration of MIDI controllers. 
    <p>    
    The \"LINE-OUT\" module
    exposes the input sockets :channel-1 ... :channel-n where n is the
    number of channels as given by the :channel-count property of the environment.
    </p><p>
    The \"MIDI-IN\" module exposes the output socket :midi-events which provides a list
    of midi-events as fired by a MIDI device.
    </p><p>
    The update function of the rack calls the update function of all its modules.
    If the rack has already been shut down it immediately returns <b>nil</b>.
    Othwerwise it returns <b>t</b>.
    </p><p>
    The shutdown function shuts the rack down by calling the shutdown handlers of all modules, devices 
    and hooks of the rack. If the rack has already been shut down the function does not call any handlers.
    </p>"
  (declare (ignore additional-input-sockets additional-output-sockets))
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (if (not environment)
      (signal-invalid-arguments-error
       :format-control "Environment must not be nil"
       :format-arguments nil))

  (let ((this nil))
    (let ((rack
	   (list
	    :environment environment
	    :is-rack t ;; TODO Do we need this flag?
	    :modules nil ;; TODO Move to outer closure context
	    :hooks nil ;; TODO Move to outer closure context
	    :rack-has-shut-down nil ;; TODO Move to outer closure context
	    :update
	    (lambda (&rest args)
	      (declare (ignore args))
	      (declare (optimize (debug 3) (speed 0) (space 0)))
	      ;;(break)
	      (if (getf this :rack-has-shut-down)
		  nil
		  (progn
		    (set-state this :PROCESS-TICK)
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
				   (let ((lambdalist nil))
				     ;; collect inputs
				     (dolist (cur-input-socket (get-rack-module-input-sockets rm))
				       (let ((patch (get-rack-module-input-patch rm cur-input-socket))
					     (socket-input-value nil))
					 (if patch
					     (let* ((source-rm (get-rack-patch-module patch))
						    (source-rm-socket (get-rack-patch-socket patch))
						    (output-fn (get-rack-module-output-fn source-rm)))
					       (setf socket-input-value (funcall output-fn source-rm-socket))))
					 (push cur-input-socket lambdalist)
					 (push socket-input-value lambdalist)))
				     ;; call update function on this
				     (apply (get-rack-module-update-fn rm) (nreverse lambdalist))
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
	      (declare (ignore socket))
	      ;; todo get output socket of output buffer
	      nil)
	    :shutdown
	    (lambda()
	      (if (not (getf this :rack-has-shut-down))
		  (progn
		    (setf (getf this :rack-has-shut-down) t)
		    (dolist (rm (getf this :modules))
		      (funcall (get-rack-module-shutdown-fn rm)))
		    (dolist (m (getf this :hooks))
		      (if (getf m :shutdown)
			  (funcall (getf m :shutdown)))))))
	    )))
      (setf this rack)

      ;; Add MIDI-IN Interface
      (add-module
       rack "MIDI-IN"
       (let ((input-args nil))
	 (lambda (name environment)
	   (declare (ignore name environment))
	   (list
	    :inputs (lambda() '(:midi-events))
	    :outputs (lambda() '(:midi-events))
	    :update (lambda (&rest args)
		      (setf input-args args)
		      nil)
	    :get-output (lambda (socket)
			  (getf input-args socket))))))

      ;; Add LINE-OUT Interface
      (add-module
       rack "LINE-OUT"
       (lambda (name environment)
	 (declare (ignore name))
	 (let ((sockets
		(cl-synthesizer-macro-util:make-keyword-list
		 "channel"
		 (getf environment :channel-count)))
	       (input-args))
	   (list
	    :inputs (lambda() sockets)
	    :outputs (lambda() sockets)
	    :update (lambda (&rest args)
		      (setf input-args args)
		      nil)
	    :get-output (lambda (socket)
			  (getf input-args socket))))))
      
      rack)))
;:
;; Rack-Module
;; 

(defclass rack-module ()
  ((state :initarg nil :documentation ":PROCESS-TICK, :PROCESSING-TICK, :TICK-PROCESSED")
   (name :initarg nil)
   (module :initarg nil)
   (input-patches :initform (make-hash-table))
   (output-patches :initform (make-hash-table)))
  (:documentation "Represents a module holding input/output connections to other modules"))

(defmethod initialize-instance :after ((rm rack-module) &key name module)
  (setf (slot-value rm 'name) name)
  (setf (slot-value rm 'module) module))

(defun get-rack-module-name (rm)
  (slot-value rm 'name))

(defun get-rack-module-state (rm)
  (slot-value rm 'state))

(defun set-rack-module-state (rm state)
  (setf (slot-value rm 'state) state))

(defun get-rack-module-input-sockets (rm)
  (let ((f (getf (slot-value rm 'module) :inputs)))
    (if f (funcall f) nil)))

(defun get-rack-module-output-sockets (rm)
  (let ((f (getf (slot-value rm 'module) :outputs)))
    (if f (funcall f) nil)))

(defun get-rack-module-update-fn (rm)
  (getf (slot-value rm 'module) :update))

(defun get-rack-module-module (rm)
  (slot-value rm 'module))

(defun get-rack-module-output-fn (rm)
  (getf (slot-value rm 'module) :get-output))

(defun get-rack-module-output (rm socket)
  (funcall (getf (slot-value rm 'module) :get-output) socket))

(defun get-rack-module-shutdown-fn (rm)
  (let ((f (getf (slot-value rm 'module) :shutdown)))
    (if f f (lambda() ()))))

(defun get-rack-module-input-patch (rm input-socket)
  (gethash input-socket (slot-value rm 'input-patches)))

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

(defun get-rack-patch-socket (patch)
  (slot-value patch 'socket))

(defun get-rack-patch-module (patch)
  (slot-value patch 'rack-module))

(defun make-rack-module-patch (rm socket)
  (let ((c (make-instance 'rack-module-patch)))
    (setf (slot-value c 'rack-module) rm)
    (setf (slot-value c 'socket) socket)
    c))

;;
;; Rack
;;

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
		    input sockets to be exposed by the module.</li>
		<li>:outputs A function with no arguments that returns a list of keywords that represent the
		    output sockets to be exposed by the module.</li>
		<li>:update A function that is called with the values of the modules input sockets
                    in order to update the state of the module (the state of its output sockets).
		    The value of each input socket is passed via a keyword parameter.</li>
		<li>:get-output A function that is called in order to get the value of a specific
		    output socket. The function is called with a keyword that identifies the output socket
		    whose state is to be returned. The function must not modify the value
		    of the given or any other output socket.</li>
		<li>:shutdown An optional function with no arguments that is called when the rack
		    is shutting down.</li>
	    </ul>
            <p>
	    The input/output sockets exposed by the module are not buffered by the rack. Therefore the
	    module should return either a quoted list or keep it in an internal variable. The module must
	    not add or remove input/output sockets after it has been instantiated.
            </p>
	</li>
	<li>&rest args Arbitrary additional arguments to be passed to the module instantiation function.
	    These arguments typically consist of keyword parameters.</li>
    </ul>"
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (if (get-rm-module rack module-name)
      (signal-assembly-error
       :format-control "A module with name ~a has already been added to the rack"
       :format-arguments (list module-name)))

  (let ((environment (getf rack :environment)))
    (let ((rm (make-instance 'rack-module
			     :module (apply module-fn `(,module-name ,environment ,@args))
			     :name module-name)))
      (push rm (getf rack :modules))
      nil)))

(defun add-patch (rack source-rm-name source-output-socket destination-rm-name destination-input-socket)
  "Adds a patch to the rack. A patch is an unidirectional connection between an output socket
    of a source module and an input socket of a destination module. The rack supports cycles which means that an
    output socket of a module can be patched with one of its inputs (typically via
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
	<ul>
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
    
    (setf (gethash destination-input-socket (slot-value destination-rm 'input-patches))
	  (make-rack-module-patch source-rm source-output-socket))
    (setf (gethash source-output-socket (slot-value source-rm 'output-patches))
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
    The function returns nil if the source module does not exist or if the source module
    does not expose the given socket or if the given socket is not connected with a module.
    Otherwise it returns a list with the following entries:
    <ul>
	<li>name Name of the destination module.</li>
	<li>module The destination module represented as a property list.</li>
	<li>socket A keyword that identifies the input or output socket of the destination
	    module. If the socket type of the source module is :input-socket then this
	    keyword represents an output socket of the destination module. Otherwise
	    it represents an input socket.
	</li>
    </ul>"
  (if (not (or (eq :input-socket socket-type) (eq :output-socket socket-type)))
      (signal-invalid-arguments-error
       :format-control "get-patch: socket must be one of :input-socket or :output-socket"
       :format-arguments nil))
  (let ((rm (get-rm-module rack module-name)))
    (if (not rm)
	nil
	(let ((patch nil))
	  (if (eq :input-socket socket-type)
	      (setf patch (get-rack-module-input-patch rm socket))
	      (setf patch (get-rack-module-output-patch rm socket)))
	  (if (not patch)
	      nil
	      (values
	       (get-rack-patch-target-name patch)
	       (get-rack-module-module (get-rack-patch-module patch))
	       (get-rack-patch-socket patch)))))))


