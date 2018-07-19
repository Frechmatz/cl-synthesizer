
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

(defun register-monitor (rack name monitor-backend outputs &rest additional-backend-args)
  "Adds a monitor to the rack. A monitor is basically a function that is called after
   each tick of the rack and to which the values of arbitrary sockets are passed.
   Monitors can for example be used to record inputs and outputs of specific
   rack modules into wave-files for debugging/analysis purposes.
   - rack: The rack
   - monitor-backend: Monitor backend handler constructor function. After validation of the request 
     this function is called in order to instantiate the monitor backend handler. 
     It is called with the following lambda list: 
     (name environment output-keywords additional-backend-args).
     The constructor function must return a property list which provides 
     the actual callback function and an optional shutdown callback.
     -- :shutdown An optional rack shutdown callback function.
     -- :update A mandatory function that is called after each tick.
     The update function is called with the following lambda list:
     (:output-key-1 value :output-key-2 value ...) 
     Output keys whose value is not defined are omitted from the callback.
   - outputs: List of output-def
     output-def := <key> <module-name> <socket>
     socket := :input-socket <input-socket-key> | :output-socket <output-socket-key>
     Example: '((:channel-1 \"ADSR\" :output-socket :cv)
                (:channel-2 \"LINE-OUT\" :input-socket :channel-1))
     For the given output declaration the update function will be
     called as follows (update-fn :channel-1 <value> :channel-2 <value>)"
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
    (let* ((backend (apply monitor-backend name (cl-synthesizer:get-environment rack) keys additional-backend-args))
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
