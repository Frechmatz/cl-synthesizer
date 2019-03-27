;;
;; Monitor
;;


(in-package :cl-synthesizer-monitor)

(defun get-module-input-patch(rack module input-socket)
  (let* ((name (cl-synthesizer:get-module-name rack module))
	 ;; list of (:output-name "name" :output-socket <socket> :input-name "name" :input-socket <socket>)
	 (patch (find-if
		 (lambda (p)
		   (and (string= name (getf p :input-name))
			(eq input-socket (getf p :input-socket))))
		 (cl-synthesizer:get-patches rack))))
	 patch))

(defun get-module-output-patch (rack module output-socket)
  (let* ((name (cl-synthesizer:get-module-name rack module))
	 ;; list of (:output-name "name" :output-socket <socket> :input-name "name" :input-socket <socket>)
	 (patch (find-if
		 (lambda (p)
		   (and (string= name (getf p :output-name))
			(eq output-socket (getf p :output-socket))))
		 (cl-synthesizer:get-patches rack))))
    patch))


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
      (cl-synthesizer:signal-invalid-arguments-error
       :format-control "get-patch: socket must be one of :input-socket or :output-socket"
       :format-arguments nil))
  (let ((rm (cl-synthesizer:get-module rack module-name)))
    (if (not rm)
	(values nil nil nil)
	(if (eq :input-socket socket-type)
	    (let ((patch (get-module-input-patch rack rm socket)))
	      (if (not patch)
		  (values nil nil nil)
		  (values
		   (getf patch :output-name)
		   (cl-synthesizer:get-module rack (getf patch :output-name))
		   (getf patch :output-socket))))
	    (let ((patch (get-module-output-patch rack rm socket)))
	      (if (not patch)
		  (values nil nil nil)
		  (values
		   (getf patch :input-name)
		   (cl-synthesizer:get-module rack (getf patch :input-name))
		   (getf patch :input-socket))))))))

(defun make-get-output-lambda (module output-socket)
  (if (not (getf module :v2))
      (let ((l (getf module :get-output)))
	(lambda() (funcall l output-socket)))
      (let ((l (getf (funcall (getf module :outputs)) output-socket)))
	(lambda() (funcall l)))))


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
		<li>module A property list that represents a subset of a module. 
                    At least :update must be implemented. See also cl-synthesizer:add-module.</li>
		<li>An ordered list of input sockets of the module, where the first entry represents 
                   the first entry of the socket mappings (e.g. column-1) and so on. This list
                   is in place because we do not want to depend on the actual input sockets
                   exposed by the module. It is up to the monitor-handler to know about 
                   specifica of modules, for example that the csv-file-writer module
                   uses input socket :column-1 to represent the first column.</li>
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
		     :format-control "Monitor: Module ~a does not expose output socket ~a"
		     :format-arguments (list module-path socket-key)))
		  (setf input-fetcher (make-get-output-lambda module socket-key)))
	       ((eq :input-socket socket-type)
		(if (not (find socket-key (funcall (getf module :inputs))))
		    (cl-synthesizer:signal-assembly-error
		     :format-control "Monitor: Module ~a does not expose input socket ~a"
		     :format-arguments (list module-path socket-key)))
		(multiple-value-bind (source-module-name source-module source-socket)
		    (get-patch module-rack module-name :input-socket socket-key)
		  (if (not source-module-name)
		      (cl-synthesizer:signal-assembly-error
		       :format-control "Monitor: Input socket exposed by module but it is not patched: ~a ~a ~a"
		       :format-arguments (list module-name socket-type socket-key)))
		  (setf input-fetcher
			;;(lambda () (funcall (getf source-module :get-output) source-socket))
			(make-get-output-lambda source-module source-socket)
			)))
	       ((eq :state socket-type)
		(let ((get-state-fn (if (getf module :get-state)
					 (getf module :get-state)
					 (lambda(key) (declare (ignore key)) nil))))
		  (setf input-fetcher (lambda() (funcall get-state-fn socket-key)))))
	       (t
		(cl-synthesizer:signal-assembly-error
		 :format-control "Monitor: Socket-Type not supported: ~a. Must be one of :input-socket, :output-socket, :state"
		 :format-arguments (list socket-type))))
	     (push (list key input-fetcher) input-fetchers)))))

       ;; Prepare input property list with which
       ;; the update function of the backend will be called.
       ;; (:INPUT-1 nil :INPUT-2 nil ...)
       (let ((backend-update-fn (getf backend :update))
	     (input-argument-list nil))
	 (dolist (p input-fetchers)
	   ;; Value
	   (push nil input-argument-list)
	   ;; Key
	   (push (first p) input-argument-list))
	 (cl-synthesizer:add-hook
	  rack
	  (list 
	   :shutdown (lambda ()
		       (if (getf backend :shutdown)
			   (funcall (getf backend :shutdown))))
	   :update (lambda()
		     (dolist (p input-fetchers)
		       (setf (getf input-argument-list (first p)) (funcall (second p))))
		     (funcall backend-update-fn input-argument-list))))))))
