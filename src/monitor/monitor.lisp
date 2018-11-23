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
		<li>inputs A list of inputs. Each entry consists of the additional
                    settings that have been set at a specific socket mapping, for example
                    the CSV formatting string.</li>
		<li>additional-handler-args Any additional keyword parameters as
		    passed to the monitor function. These parameters can be
		    used to initialize handler specific properties such as
		    a filename.</li>
	    </ul>
	    The function must return a values object with the following entries:
	    <ul>
		<li>module A property list that represents a module. See also cl-synthesizer:add-module.</li>
		<li>An ordered list of input keys of the module, where the first key represents 
                   the first entry of the socket mappings (e.g. column-1) and so on.</li>
	    </ul>
	</li>
	<li>socket-mappings Declares the input/outputs whose values are to be monitored.
            Each entry has the following format:
	    <ul>
		<li>module-path Path of the module from which the value of
		    a certain input/output socket or state is to be retrieved, for
		    example \"ADSR\" or '(\"VOICE-1\" \"ADSR\"). 
                    See also cl-synthesizer:find-module.</li>
		<li>socket-type One of the following keywords: 
                    <ul>
                        <li>:input-socket Monitor the value of an input socket of the module.</li>
                        <li>:output-socket Monitor the value of an output socket of the module.</li>
                        <li>:state Monitor a state of the module (see get-state function).</li>
                    </ul>
                </li>
		<li>socket A keyword that identifies one of the input/output sockets or states
		    provided by the module, for example :cv</li>
                <li>Any additional settings. Supported settings depend
                    on the handler that is being used, for example a CSV writer may
                    support a column formatting string.</li>
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
	     ;; ("OUTPUT" :input-socket :line-out :extra-1 "Extra") => (:extra-1 "Extra") 
	     (cdr (cdr (cdr m))))
	   socket-mappings)
	  additional-handler-args)
       (dotimes (i (length socket-mappings))
	 (let* ((key (nth i ordered-input-sockets)) ;; input socket key defined by backend
		(socket-mapping (nth i socket-mappings))
		(module-path (first socket-mapping))
		(socket-type (second socket-mapping))
		(socket-key (third socket-mapping)))
	   (multiple-value-bind (module-rack module-name module)
	       (cl-synthesizer:find-module rack module-path)
	   (if (not module)
	       (cl-synthesizer:signal-assembly-error
		:format-control "Monitor: Cannot find module ~a"
		:format-arguments (list module-path)))
	   (let ((input-fetcher nil))
	     (cond
	       ((eq :output-socket socket-type)
		(if (not (find socket-key (funcall (getf module :outputs))))
		    (cl-synthesizer:signal-assembly-error
		     :format-control "Monitor: Module ~a does not have output socket ~a"
		     :format-arguments (list module-path socket-key)))
		(let ((get-output-fn (getf module :get-output)))
		  (setf input-fetcher (lambda() (funcall get-output-fn socket-key)))))
	       ((eq :input-socket socket-type)
		(multiple-value-bind (source-module-name source-module source-socket)
		    (cl-synthesizer:get-patch module-rack module-name :input-socket socket-key)
		  (if (not source-module-name)
		      (cl-synthesizer:signal-assembly-error
		       :format-control "Monitor: Socket not patched or not exposed by module: ~a ~a ~a"
		       :format-arguments (list module-name socket-type socket-key)))
		  (setf input-fetcher
			(lambda () (funcall (getf source-module :get-output) source-socket)))))
	       ((eq :state socket-type)
		(let ((get-state-fn (if (getf module :get-state)
					 (getf module :get-state)
					 (lambda(key) (declare (ignore key)) nil))))
		  (setf input-fetcher (lambda() (funcall get-state-fn socket-key)))))
	       (t
		(cl-synthesizer:signal-assembly-error
		 :format-control "Monitor: Socket-Type not supported: ~a. Must be one of :input-socket, :output-socket"
		 :format-arguments (list socket-type))))
	     (push (list key input-fetcher) input-fetchers)))))

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
