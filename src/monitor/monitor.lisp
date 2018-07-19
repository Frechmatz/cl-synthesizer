
(in-package :cl-synthesizer-monitor)

(defun validate-output (rack output output-sockets)
  (let ((key (first output))
	(module-name (second output))
	(socket-type (third output))
	(socket-key (fourth output)))
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
    ;; get-module needs to be fixed back rack API rework
    (if (not (cl-synthesizer::get-module rack module-name))
	(cl-synthesizer:signal-assembly-error
	 :format-control "Monitor: Cannot find module ~a"
	 :format-arguments (list module-name)))
    (if (eq :output-socket socket-type)
	(if (not (find socket-key (cl-synthesizer:get-module-output-sockets rack module-name)))
	    (cl-synthesizer:signal-assembly-error
	     :format-control "Module ~a does not have output socket ~a"
	     :format-arguments (list module-name socket-key))))
    (if (eq :input-socket socket-type)
	(if (not (cl-synthesizer:get-input-module-name rack module-name socket-key))
	    (cl-synthesizer:signal-assembly-error
	     :format-control "Monitor: Input socket ~a of module ~a is not connected with a module"
	     :format-arguments (list socket-key module-name ))))))

(defun add-monitor (rack monitor-backend outputs &rest additional-backend-args)
  "Adds a monitor to the rack. A monitor is a high-level Rack hook that is called
    after the Rack has processed a tick. When the monitor is being called by the
    Rack it collects the values of arbitray input and output sockets of any modules
    of the rack and then passes these values to a monitor backend handler.
    A monitor backend can for example be a Wave-File-Writer or a CSV file output
    module.
    The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
	<li>monitor-backend A function that instantiates the monitor backend.
	    This function is called with the following lambda list: 
	    (name environment output-keywords additional-backend-args).
	    The constructor function must return a property list with
	    the following keys:
	    <ul>
		<li>:shutdown An optional function without arguments that is
		    called when the rack shuts down</li>
		<li>:update A mandatory function that is called after each tick.
		    This function is called with the following lambda list:
		    (:output-keyword-1 value :output-keyword-2 value ...)
		    output-keywords whose value is not defined are omitted by
		    the Monitor.</li>
	    </ul>
	</li>
	<li>outputs Declares the sockets whose values are to be passed to
	    the backend. The outputs consist of a list of output definitions, where
	    each output definition has the following lambda list:
	    output-def := key module-name socket
	    socket := :input-socket input-socket-key | :output-socket output-socket-key
	    Example: '((:channel-1 \"ADSR\" :output-socket :cv)
	    (:channel-2 \"LINE-OUT\" :input-socket :channel-1))
	</li>
	<li>&rest additional-backend-args Optional arguments that are passed to
	    the monitor-backend on instantiation.</li>
    </ul>
    Example using the wave-file-handler monitor backend:
    <pre><code>
    (cl-synthesizer-monitor:add-monitor
     rack
     #'cl-synthesizer-monitor-wave-handler:wave-file-handler
     '((:channel-1 \"ADSR\" :output-socket :cv)
       (:channel-2 \"LINE-OUT\" :input-socket :channel-1)
       (:channel-3 \"LFO\" :output-socket :saw))
     :filename \"/Users/olli/waves/envelope.wav\")
    </code></pre>"
  (let ((output-handlers nil) ;; list of (keyword lambda) 
	(keys nil))
    (dolist (output outputs)
      (validate-output rack output keys)
      (let ((key (first output))
	    (module-name (second output))
	    (socket-type (third output))
	    (socket-key (fourth output)))
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
			(if v ;; Omit from monitor callback if not defined
			    (progn
			      (push v params)
			      (push (first p) params)))))
		    (apply update-fn params))))))))
