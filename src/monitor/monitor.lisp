;;
;; Monitor
;;


(in-package :cl-synthesizer-monitor)

(defun get-module-input-patch (rack module input-socket)
  (let* ((name (cl-synthesizer:get-module-name rack module))
	 ;; list of (:output-name "name" :output-socket <socket> :input-name "name" :input-socket <socket>)
	 (patch
	  (find-if
	   (lambda (p)
	     (and (string= name (getf p :input-name))
		  (eq input-socket (getf p :input-socket))))
	   (cl-synthesizer:get-patches rack))))
    patch))

(defun get-module-output-patch (rack module output-socket)
  (let* ((name (cl-synthesizer:get-module-name rack module))
	 ;; list of (:output-name "name" :output-socket <socket> :input-name "name" :input-socket <socket>)
	 (patch
	  (find-if
	   (lambda (p)
	     (and (string= name (getf p :output-name))
		  (eq output-socket (getf p :output-socket))))
	   (cl-synthesizer:get-patches rack))))
    patch))

(defun get-patch (rack module-name socket-type socket)
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
  (let ((l (getf (funcall (getf module :outputs)) output-socket)))
    (lambda() (funcall l))))


(defun get-input-fetcher (rack socket-mapping)
  "Returns a lambda that returns the actual value of an input, output or state
   of the module designated by the socket mapping"
  (let* ((module-path (first socket-mapping))
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
	input-fetcher))))

(defun add-monitor (rack monitor-backend socket-mappings &rest additional-handler-args)
   "Adds a monitor to a rack. <p>The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
	<li>monitor-backend A function that instantiates the monitor backend.
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
		<li>module A property list that implements a module (this is the Monitor-Backend).</li>
		<li>An ordered list of input sockets of the module (the Monitor-Backend), where the first entry represents 
                   the first entry of the socket mappings (e.g. column-1) and so on.</li>
	    </ul>
	</li>
	<li>socket-mappings Declares the input/outputs/states whose values are to be tracked.
            Each entry has the following format:
	    <ul>
		<li>module-path Path of the module from which the value of
		    a certain input/output socket or state is to be retrieved, for
		    example \"ADSR\" or '(\"VOICE-1\" \"ADSR\"). 
                    See also cl-synthesizer:find-module.</li>
		<li>socket-type One of the following keywords: 
                    <ul>
                        <li>:input-socket The value of an input socket of the module.</li>
                        <li>:output-socket The value of an output socket of the module.</li>
                        <li>:state The value of an internal state of the module (see get-state function).</li>
                    </ul>
                </li>
		<li>socket A keyword that identifies one of the input/output sockets or internal states
		    provided by the module, for example :cv</li>
                <li>Any additional settings. Supported settings depend
                    on the handler that is being used, for example a CSV writer may
                    support a column formatting string.</li>
	    </ul>
	</li>
	<li>&rest additional-handler-args Optional keyword arguments to be passed to
	    the handler instantiation function.</li>
    </ul></p>"
     ;; Call the monitor handler to get a backend (which is a module) and an
     ;; ordered list of input sockets of the backend. We cannot
     ;; depend here on the order of the input sockets that is exposed by the backend,
     ;; because modules are not required to expose their input sockets in any specific order.
     ;; Defining the positional mapping is job of the monitor handler.
   (multiple-value-bind (backend ordered-input-sockets)
       (apply
	monitor-backend
	"Monitor-Backend-Ctor"
	(cl-synthesizer:get-environment rack)
	(mapcar
	 (lambda(m)
	   ;; ("OUTPUT" :input-socket :line-out :extra-1 "Extra") => (:extra-1 "Extra") 
	   (cdr (cdr (cdr m))))
	 socket-mappings)
	additional-handler-args)

     (if (not backend)
	 (cl-synthesizer:signal-assembly-error
	  :format-control "Monitor: Backend handler must not be nil"
	  :format-arguments nil))

     (if (not (functionp (getf backend :inputs)))
	 (cl-synthesizer:signal-assembly-error
	  :format-control "Monitor: Backend must provide an inputs function"
	  :format-arguments nil))
     
     (if (not (functionp (getf backend :update)))
	 (cl-synthesizer:signal-assembly-error
	  :format-control "Monitor: Backend must provide an update function"
	  :format-arguments nil))
     
     (if (not ordered-input-sockets)
	 (cl-synthesizer:signal-assembly-error
	  :format-control "Monitor: Sockets returned by handler must not be nil"
	  :format-arguments nil))

     (if (not (= (length ordered-input-sockets) (length socket-mappings)))
	 (cl-synthesizer:signal-assembly-error
	  :format-control
	  "Monitor: Number of sockets (~a) returned by handler must not differ from number of socket mappings: ~a"
	  :format-arguments (list (length ordered-input-sockets) (length socket-mappings))))

     (let ((set-input-lambdas (make-array (length socket-mappings) :initial-element nil))
	   (backend-inputs (funcall (getf backend :inputs)))
	   (backend-update (getf backend :update))
	   (socket-count (length socket-mappings)))
       ;; Set up lambdas for setting the inputs of the backend
       (dotimes (i socket-count)
	 (let* ((backend-input-socket (nth i ordered-input-sockets))
		(socket-mapping (nth i socket-mappings)))
	   (let ((input-fetcher (get-input-fetcher rack socket-mapping))
		 (input-setter (getf backend-inputs backend-input-socket)))
	     (setf (elt set-input-lambdas i)
		   (lambda()
		     (funcall input-setter (funcall input-fetcher)))))))
       ;; Compile
       (let ((compiled-backend-update
	      (lambda()
		;; Set inputs
		(dotimes (index socket-count)
		  (funcall (elt set-input-lambdas index)))
		;; Update
		(funcall backend-update))))
	 (cl-synthesizer:add-hook
	  rack
	  (list 
	   :shutdown (lambda ()
		       (if (getf backend :shutdown)
			   (funcall (getf backend :shutdown))))
	   :update compiled-backend-update))))))
