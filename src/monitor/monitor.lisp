
(in-package :cl-synthesizer-monitor)

(defun validate-output (rack socket-mapping output-sockets)
  (let ((key (first socket-mapping))
	(module-name (second socket-mapping))
	(socket-type (third socket-mapping))
	(socket-key (fourth socket-mapping)))
    (if (not (keywordp key))
	(cl-synthesizer:signal-assembly-error
	 :format-control "Monitor: Key must be a keyword ~a"
	 :format-arguments (list key)))
    (if (find key output-sockets :test #'eq)
	(cl-synthesizer:signal-assembly-error
	 :format-control "Monitor: Key already used ~a"
	 :format-arguments (list key)))
    (if (and (not (eq :output-socket socket-type)) (not (eq :input-socket socket-type)))
	(cl-synthesizer:signal-assembly-error
	 :format-control "Monitor: Invalid socket type: ~a Must be one of :input-socket, :output-socket"
	 :format-arguments (list socket-type)))
    ;; get-rm-module needs to be fixed back rack API rework
    (if (not (cl-synthesizer:get-module rack module-name))
	(cl-synthesizer:signal-assembly-error
	 :format-control "Monitor: Cannot find module ~a"
	 :format-arguments (list module-name)))
    (if (eq :output-socket socket-type)
	(let ((module (cl-synthesizer:get-module rack module-name)))
	  (if (not (find socket-key (funcall (getf module :outputs))))
	    (cl-synthesizer:signal-assembly-error
	     :format-control "Module ~a does not have output socket ~a"
	     :format-arguments (list module-name socket-key)))))
    (if (eq :input-socket socket-type)
	(if (not (cl-synthesizer:get-input-socket-patch rack module-name socket-key))
	    (cl-synthesizer:signal-assembly-error
	     :format-control "Monitor: Input socket ~a of module ~a is not connected with a module"
	     :format-arguments (list socket-key module-name ))))))

(defun add-monitor (rack monitor-backend socket-mappings &rest additional-backend-args)
  "    Adds a monitor to a rack. A monitor is a high-level Rack hook that
    collects module states (input/output sockets) and passes them
    to a monitor backend. A monitor backend can for example be a
    Wave-File-Writer. The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
	<li>monitor-backend A function that instantiates the monitor backend.
	    This function is called with the following arguments:
	    <ul>
		<li>name A name.</li>
		<li>environment The synthesizer environment.</li>
		<li>output-keywords A list of keywords declaring the keyword
		    parameters with which the monitor backend update function
		    will be called.</li>
		<li>additional-backend-args Any additional keyword parameters as
		    passed to the monitor function. These parameters can be
		    used to initialize backend specific properties such as
		    a filename.</li>
	    </ul>
	    The function must return a property list with the following keys:
	    <ul>
		<li>:update A function that is called after each tick of the rack.
		    It is called with keyword parameters as declared by the
		    output-keywords argument described above.</li>
		<li>:shutdown An optional function with no arguments that is
		    called when the rack shuts down.</li>
	    </ul>
	</li>
	<li>socket-mappings Declares a list of mappings of specific sockets of
	    specific modules to keyword parameters that will be passed to the
	    update function of the backend. Each mapping entry has the following format:
	    <ul>
		<li>key Keyword to be used as keyword parameter when the backend
		    update function is called, for example :channel-1.
		    For now this key must be one that is supported by the backend. For
		    example the Wave-File-Writer backend only understands :channel-n
		    keys. This might be changed in a later point of time.</li>
		<li>module-name Name of the module from which the state of
		    a certain input/output socket is to be retrieved, for
		    example \"ADSR\"</li>
		<li>socket-type Defines if the value of an input-socket is requested
		    or the value of an output-socket. Must be :input-socket or
		    :output-socket</li>
		<li>socket A keyword that identifies one of the input/output sockets
		    as provided by the module, for example :cv</li>
	    </ul>
	</li>
	<li>&rest additional-backend-args Optional keyword arguments to be passed to
	    the backend instantiation function.</li>
    </ul>
    Example using the wave-file-handler monitor backend:
    <pre><code>
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 \"LFO\" :output-socket :saw)
       (:channel-2 \"ADSR\" :output-socket :cv)
       (:channel-3 \"LINE-OUT\" :input-socket :channel-1))
     :filename \"trace.wav\")
    </code></pre>"
  (let ((output-handlers nil)
	(keys nil))
    (dolist (socket-mapping socket-mappings)
      (validate-output rack socket-mapping keys)
      (let ((key (first socket-mapping))
	    (module-name (second socket-mapping))
	    (socket-type (third socket-mapping))
	    (socket-key (fourth socket-mapping)))
	(push key keys)
	(let ((handler nil))
	  (if (eq :output-socket socket-type)
	      (setf handler (lambda () (cl-synthesizer:get-module-output rack module-name socket-key)))
	      (setf handler (lambda () (cl-synthesizer:get-module-input rack module-name socket-key))))
	  (push (list key handler)  output-handlers))))
    (let* ((backend (apply
		     monitor-backend
		     "Monitor-Backend"
		     (cl-synthesizer:get-environment rack) keys additional-backend-args))
	   (update-fn (getf backend :update)))
      (cl-synthesizer:add-hook
       rack
       (list 
	:shutdown (lambda () (if (getf backend :shutdown) (funcall (getf backend :shutdown))))
	:update (lambda()
		  ;; TODO Params list should not be created on each update call
		  (let ((params nil))
		    (dolist (p output-handlers)
		      (let ((v (funcall (second p))))
			;; Value
			(push v params)
			;; Key
			(push (first p) params)))
		    (apply update-fn params))))))))
