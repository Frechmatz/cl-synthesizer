;;
;; Monitor
;;


(in-package :cl-synthesizer-monitor)

(defun add-monitor (rack monitor-handler socket-mappings &rest additional-handler-args)
   "Adds a monitor to a rack. A monitor is a high-level Rack hook that
    collects module states (values of input/output sockets) and passes them
    to a monitor handler. A monitor handler can for example be a
    Wave-File-Writer. The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
	<li>monitor-handler A function that instantiates the monitor handler.
	    This function is called with the following arguments:
	    <ul>
		<li>name A name.</li>
		<li>environment The synthesizer environment.</li>
		<li>inputs A list of inputs. Each input consists of a property
                    list with the following keys:
                    <ul>
                        <li>:input-socket Keyword that is used as keyword input parameter 
                        when the update function of the handler is called.</li>
                        <li>:settings Any additional settings specified by a 
                        socket mapping entry.</li>
                    </ul>
                </li>
		<li>additional-handler-args Any additional keyword parameters as
		    passed to the monitor function. These parameters can be
		    used to initialize handler specific properties such as
		    a filename.</li>
	    </ul>
	    The function must return a property list with the following keys:
	    <ul>
		<li>:update A function that is called after each tick of the rack.
		    It is called with keyword parameters as declared by the
		    input-keywords argument described above.</li>
		<li>:shutdown An optional function with no arguments that is
		    called when the rack shuts down.</li>
	    </ul>
	</li>
	<li>socket-mappings Declares a list of mappings of specific sockets of
	    specific modules to keyword parameters that will be passed to the
	    update function of the handler. Each mapping entry has the following format:
	    <ul>
		<li>key Keyword to be used as keyword input parameter when calling
		    the update function of the handler, for example :channel-1.
		    For now this key must be one that is supported by the actual handler. 
                    For example the Wave-File handler only supports input keys
                    :channel-1 ... :channel-n.</li>
		<li>module-name Name of the module from which the state of
		    a certain input/output socket is to be retrieved, for
		    example \"ADSR\"</li>
		<li>socket-type Defines if the value of an input-socket is to be
                    passed to the handler or the value of an output-socket. 
                    Must be :input-socket or :output-socket</li>
		<li>socket A keyword that identifies one of the input/output sockets
		    provided by the module, for example :cv</li>
                <li>Any additional socket mapping settings. These will be passed to the 
                    handler instantiation function.</li>
	    </ul>
	</li>
	<li>&rest additional-handler-args Optional keyword arguments to be passed to
	    the handler instantiation function.</li>
    </ul>"
   (let ((input-fetchers nil))
     (multiple-value-bind (backend ordered-input-sockets)
	 (apply
	  monitor-handler
	  "Monitor-Handler"
	  (cl-synthesizer:get-environment rack)
	  (mapcar
	   (lambda(m)
	     ;; (:channel-1 "OUTPUT" :input-socket :line-out :extra-1 "Extra") => (:extra-1 "Extra") 
	     (cdr (cdr (cdr (cdr m)))))
	   socket-mappings)
	  additional-handler-args)
       (dotimes (i (length socket-mappings))
	 (let* ((key (nth i ordered-input-sockets)) ;; input socket key defined by backend
		(socket-mapping (nth i socket-mappings))
		(module-name (second socket-mapping))
		(socket-type (third socket-mapping))
		(socket-key (fourth socket-mapping))
		(module (cl-synthesizer:get-module rack module-name)))
	   (if (not module)
	       (cl-synthesizer:signal-assembly-error
		:format-control "Monitor: Cannot find module ~a"
		:format-arguments (list module-name)))
	   (let ((input-fetcher nil))
	     (cond
	       ((eq :output-socket socket-type)
		(if (not (find socket-key (funcall (getf module :outputs))))
		    (cl-synthesizer:signal-assembly-error
		     :format-control "Monitor: Module ~a does not have output socket ~a"
		     :format-arguments (list module-name socket-key)))
		(let ((get-output-fn (getf module :get-output)))
		  (setf input-fetcher (lambda() (funcall get-output-fn socket-key)))))
	       ((eq :input-socket socket-type)
		(multiple-value-bind (source-module-name source-module source-socket)
		    (cl-synthesizer:get-patch rack module-name :input-socket socket-key)
		  (if (not source-module-name)
		      (cl-synthesizer:signal-assembly-error
		       :format-control "Monitor: Socket not patched or not exposed by module: ~a ~a ~a"
		       :format-arguments (list module-name socket-type socket-key)))
		  (setf input-fetcher
			(lambda () (funcall (getf source-module :get-output) source-socket)))))
	       (t
		(cl-synthesizer:signal-assembly-error
		 :format-control "Monitor: Socket-Type not supported: ~a. Must be one of :input-socket, :output-socket"
		 :format-arguments (list socket-type))))
	     (push (list key input-fetcher) input-fetchers))))

       (let* ((backend-update-fn (getf backend :update)))
	 (cl-synthesizer:add-hook
	  rack
	  (list 
	   :shutdown (lambda ()
		       (if (getf backend :shutdown)
			   (funcall (getf backend :shutdown))))
	   :update (lambda()
		     (let ((params nil))
		       (dolist (p input-fetchers)
			 (let ((v (funcall (second p))))
			   ;; Value
			   (push v params)
			   ;; Key
			   (push (first p) params)))
		       (apply backend-update-fn params)))))))))
