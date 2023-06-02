;;
;; Monitor
;;


(in-package :cl-synthesizer-monitor)

(defun assert-backend-module-structure (module)
  (labels ((iterate-list (l callback)
	     (dotimes (i (/ (length l) 2))
	       (let ((element-1 (nth (* i 2) l))
		     (element-2 (nth (+ (* i 2) 1) l)))
		 (funcall callback element-1 element-2))))
	   (assert-plistp (plist format-control format-arguments)
	     (if (or (not (listp plist)) (not (evenp (length plist))))
		 (error
		  'cl-synthesizer:assembly-error
		  :format-control format-control
		  :format-arguments format-arguments))
	     (iterate-list
	      plist
	      (lambda (keyword value)
		(declare (ignore value))
		(if (not (keywordp keyword))
		    (error
		     'cl-synthesizer:assembly-error
		     :format-control format-control
		     :format-arguments format-arguments))))))
    (if (not module)
	(error
	 'cl-synthesizer:assembly-error
	 :format-control "Monitor: Backend module must not be nil"))
    
    (assert-plistp
     module
     "Monitor: Backend module is not a property list: ~a" (list module))

    ;;
    ;; Check update function
    ;;
    (if (not (functionp (getf module :update)))
	(error
	 'cl-synthesizer:assembly-error
	 :format-control "Monitor: Invalid backend module: Property :update must be a function"
	 :format-arguments (list module)))
    
    ;;
    ;; Check inputs
    ;;
    (if (not (functionp (getf module :inputs)))
	(error
	 'cl-synthesizer:assembly-error
	 :format-control "Monitor: Invalid backend module: Property :inputs must be a function (~a)"
	 :format-arguments (list module)))
    (let ((inputs (funcall (getf module :inputs))))
      (assert-plistp
       inputs
       "Monitor: Inputs of backend module are not a property list: ~a"
       (list inputs))
      ;; check all inputs
      (iterate-list
       inputs
       (lambda (socket socket-properties)
	 (assert-plistp
	  socket-properties
	  "Monitor: Socket properties of input socket ~a are not a property list: ~a"
	  (list socket socket-properties))
	 ;; check presence of setter
	 (if (not (getf socket-properties :set))
	     (error
	      'cl-synthesizer:assembly-error
	      :format-control
	      "Monitor: Input socket ~a of backend module does not have a 'set' function"
	      :format-arguments (list socket)))
	 ;; check if setter is a function
	 (if (not (functionp (getf socket-properties :set)))
	     (error
	      'cl-synthesizer:assembly-error
	      :format-control "Monitor: Setter of input socket ~a is not a function"
	      :format-arguments (list socket))))))))

(defun get-module (rack module-path)
  (if module-path
      (cl-synthesizer:find-module rack module-path)
      rack))

(defun get-input-getter (rack socket-mapping)
  "Returns a lambda that returns the actual value of an input, output or state
   of the module designated by the socket mapping"
  (let* ((module-path (first socket-mapping))
	 (socket-type (second socket-mapping))
	 (socket-key (third socket-mapping)))
    (let ((module (get-module rack module-path)))
      (if (not module)
	  (error
	   'cl-synthesizer:assembly-error
	   :format-control "Monitor: Cannot find module '~a'"
	   :format-arguments (list module-path)))
      (let ((input-fetcher nil))
	(cond
	  ((eq :output-socket socket-type)
	   (if (not (find socket-key (funcall (getf module :outputs))))
	       (error
		'cl-synthesizer:assembly-error
		:format-control "Monitor: Module '~a' does not expose output socket '~a'"
		:format-arguments (list module-path socket-key)))
	   (setf input-fetcher (getf (getf (funcall (getf module :outputs)) socket-key) :get)))
	  ((eq :input-socket socket-type)
	   (if (not (find socket-key (funcall (getf module :inputs))))
	       (error
		'cl-synthesizer:assembly-error
		:format-control "Monitor: Module '~a' does not expose input socket '~a'"
		:format-arguments (list module-path socket-key)))
	   (setf input-fetcher (getf (getf (funcall (getf module :inputs)) socket-key) :get)))
	  ((eq :state socket-type)
	   (let ((get-state-fn (if (getf module :state)
				   (getf module :state)
				   (lambda(key) (declare (ignore key)) nil))))
	     (setf input-fetcher (lambda() (funcall get-state-fn socket-key)))))
	  (t
	   (error
	    'cl-synthesizer:assembly-error
	    :format-control "Monitor: Socket-Type not supported: '~a'. Must be one of :input-socket, :output-socket, :state"
	    :format-arguments (list socket-type))))
	input-fetcher))))

(defun add-monitor (rack monitor-agent socket-mappings &rest additional-agent-args)
  "Registers a monitor at a rack.<p>The function has the following parameters:
    <ul>
	<li>rack The rack.</li>
	<li>monitor-agent A function that instantiates the monitor backend.
	    This function is called with the following parameters:
	    <ul>
		<li>name A name.</li>
		<li>environment The synthesizer environment.</li>
		<li>inputs A list of inputs. Each entry consists of the additional
                    settings that have been set at a specific socket mapping, for example
                    the CSV formatting string.</li>
		<li>additional-agent-args Any additional keyword parameters as
		    passed to the monitor function. These parameters can be
		    used to initialize agent specific properties such as
		    the filename of a CSV file.</li>
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
		<li>module-path Optional path of a module of the rack from which the value of
		    a certain input/output socket or state is to be retrieved, for
		    example \"ADSR\" or '(\"VOICE-1\" \"ADSR\").</li>
		<li>socket-type One of the following keywords: 
                    <ul>
                        <li>:input-socket The value of an input socket of the module.</li>
                        <li>:output-socket The value of an output socket of the module.</li>
                        <li>:state The value of an internal state of the module (see state function).</li>
                    </ul>
                </li>
		<li>socket A keyword that identifies one of the input/output sockets or internal states
		    provided by the module, for example :cv</li>
                <li>Any additional settings. Supported settings depend
                    on the agent that is being used, for example a CSV writer may
                    support a column name.</li>
	    </ul>
	</li>
	<li>&rest additional-agent-args Optional keyword arguments to be passed to
	    the agent.</li>
    </ul></p>"
  ;; Instantiate backend
  (multiple-value-bind (backend ordered-input-sockets)
      (apply
       monitor-agent
       "Monitor-Backend"
       (cl-synthesizer:get-environment rack)
       (mapcar
	(lambda(m)
	  (cdr (cdr (cdr m))))
	socket-mappings)
       additional-agent-args)

    (assert-backend-module-structure backend)
    
    (if (not ordered-input-sockets)
	(error
	 'cl-synthesizer:assembly-error
	 :format-control "Monitor: Sockets returned by agent must not be nil"
	 :format-arguments nil))

    (if (not (= (length ordered-input-sockets) (length socket-mappings)))
	(error
	 'cl-synthesizer:assembly-error
	 :format-control
	 "Monitor: Number of sockets ('~a') returned by agent does not match number of socket mappings: ~a"
	 :format-arguments (list (length ordered-input-sockets) (length socket-mappings))))

    (let ((set-input-lambdas (make-array (length socket-mappings) :initial-element nil))
	  (backend-inputs (funcall (getf backend :inputs)))
	  (backend-update (getf backend :update))
	  (socket-count (length socket-mappings)))
      ;; Set up lambdas for setting the inputs of the backend
      (dotimes (i socket-count)
	(let* ((backend-input-socket (nth i ordered-input-sockets))
	       (socket-mapping (nth i socket-mappings)))
	  (let ((input-fetcher (get-input-getter rack socket-mapping))
		(input-setter (getf (getf backend-inputs backend-input-socket) :set)))
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
		      (cl-synthesizer:shutdown backend))
	  :updated compiled-backend-update))))))
