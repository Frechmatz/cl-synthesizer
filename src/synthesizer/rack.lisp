(in-package :cl-synthesizer)

;;
;;
;; Rack
;;
;; Represents a grid of rack-modules
;;
;; Todo: Re-think shutdown concept
;;

(defclass rack ()
  ((modules :initform nil)
   (environment :initform nil)
   ;; :name name :callbackFn fn
   (event-listeners :initform nil))
  (:documentation ""))

(defmethod initialize-instance :after ((r rack) &key environment)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (if (not environment)
      (error "Environment must not be nil"))
  (setf (slot-value r 'environment) environment))

(defun add-event-listener (rack name event-handler-fn &key (tick-fn nil) (shutdown-fn nil))
  "Add an event listener
   event-handler-fn -- a function being called when a module fires an event. Has the 
   following parameters:
   --- event-id -- A keyword representing a unique identifier of the event. The event-id
       is created when a module registers an event.
       consists of concatenated mdou
   --- module-name 
   --- event-name
   tick-fn -- an optional function being called once per update cycle of the rack
   shutdown-fn -- an optional function being called when the rack is shutting down"
    (if (not name)
	(error "addEventListener: no name given"))
    (if (not event-handler-fn)
	(error "addEventListener: no event callback function given"))
    ;; todo: check if listener of given name already exists
    (push (list
	   :event-handler-fn event-handler-fn
	   :tick-fn tick-fn
	   :shutdown-fn shutdown-fn)
	  (slot-value rack 'event-listeners)))

(defmacro with-event-listeners (rack callback-selector handler &body body)
  (let ((l (gensym)))
    `(dolist (,l (slot-value ,rack 'event-listeners))
       (let ((,handler (getf ,l ,callback-selector)))
	 (if handler
	     (progn ,@body))))))

(defun assert-is-module-name-available (rack name)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (if (get-module rack name)
      (signal-assembly-error
       :format-control "Module name ~a is not available"
       :format-arguments (list name))))

(defun push-alist (alist key value)
  (push value alist)
  (push key alist)
  alist)

(defun add-module (rack module-name module-fn &rest args)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (assert-is-module-name-available rack module-name)
  ;; Copy environment and inject function that allows a module to register events
  (let ((module-environment (copy-list (slot-value rack 'environment))))
    (setf module-environment
	  (push-alist
	   module-environment
	   :register-event
	   (lambda (event-name)
	     (let ((event-id
		     (intern (format nil "~a-~a" (string-upcase module-name) (string-upcase event-name)) "KEYWORD")))
	       (lambda ()
		 (with-event-listeners rack :event-handler-fn handler
		   (funcall handler event-id module-name event-name)))
	       ))))
    (let ((rm (make-instance 'rack-module)) (m (apply module-fn `(,module-environment ,@args))))
      (setf (slot-value rm 'name) module-name)
      (setf (slot-value rm 'module) m)
      (push rm (slot-value rack 'modules))
      rm)))

(defun get-module (rack name)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (find-if (lambda (rm) (string= name (get-rack-module-name rm))) (slot-value rack 'modules)))

(defun set-state (rack state)
  (dolist (m (slot-value rack 'modules))
    (setf (slot-value m 'state) state)))

(defun assert-is-module-output-socket (rm socket)
  (if (not (find socket (get-rack-module-output-sockets rm)))
      (signal-assembly-error
              :format-control "Module ~a does not have output socket ~a"
              :format-arguments (list (get-rack-module-name rm) socket))))

(defun assert-is-module-input-socket (rm socket)
  (if (not (find socket (get-rack-module-input-sockets rm)))
      (signal-assembly-error
       :format-control "Module ~a does not have input socket ~a"
       :format-arguments (list (get-rack-module-name rm) socket))))

(defun assert-input-socket-unoccupied (rm socket)
  (let ((i (get-rack-module-input-patch rm socket)))
    (if i
	(signal-assembly-error
	 :format-control "Input socket ~a of module ~a is already connected to output socket ~a of module ~a"
	 :format-arguments (list
			    socket
			    (get-rack-module-name rm)
			    (get-rack-patch-target-name i)
			    (get-rack-patch-socket i))))))

(defun assert-output-socket-unoccupied (rm socket)
  (let ((i (get-rack-module-output-patch rm socket)))
    (if i
	(signal-assembly-error
	 :format-control "Output socket ~a of module ~a is already connected to input socket ~a of module ~a"
	 :format-arguments (list
			    socket
			    (get-rack-module-name rm)
			    (get-rack-patch-socket i)
			    (get-rack-patch-target-name i))))))

(defun add-patch (rack source-rm-name source-output-socket destination-rm-name destination-input-socket)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((source-rm (get-module rack source-rm-name))
	(destination-rm (get-module rack destination-rm-name)))
    (if (not source-rm)
	(signal-assembly-error
	 :format-control "Cannot find source module ~a"
	 :format-arguments (list source-rm-name)))
    (if (not destination-rm)
	(signal-assembly-error
	 :format-control "Cannot find destination module ~a"
	 :format-arguments (list destination-rm-name)))
    (assert-is-module-output-socket source-rm source-output-socket)
    (assert-is-module-input-socket destination-rm destination-input-socket)
    (assert-input-socket-unoccupied destination-rm destination-input-socket)
    (assert-output-socket-unoccupied source-rm source-output-socket)
    (setf (gethash destination-input-socket (slot-value destination-rm 'input-patches))
	  (make-rack-module-patch source-rm source-output-socket))
    (setf (gethash source-output-socket (slot-value source-rm 'output-patches))
	  (make-rack-module-patch destination-rm destination-input-socket))))

(defun update-rack (rack)
  "Process a tick" 
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (set-state rack :PROCESS-TICK)
  (labels
      ((update-rm (rm)
	 ;; Update a module
	 ;; If module is already updating do nothing
	 ;; Otherwise update all input modules and then update outputs
	 (declare (optimize (debug 3) (speed 0) (space 0)))
	 (let ((state (get-rack-module-state rm)))
	   (if (not (eq state :PROCESS-TICK))
	       (progn
		 ;; module is already processing -> do nothing
		 ;; (break)
		 nil)
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
		     (let ((patch (get-rack-module-input-patch rm cur-input-socket)) (socket-input-value nil))
		       (if patch
			   (let* ((source-rm (get-rack-patch-module patch))
				  (source-rm-socket (get-rack-patch-socket patch))
				  (output-fn (get-rack-module-output-fn source-rm)))
			     (setf socket-input-value (funcall output-fn source-rm-socket))))
		       ;; omit from lambdalist if input is not connected or input-value is undefined
		       (if socket-input-value
			   (progn 
			     (push cur-input-socket lambdalist)
			     (push socket-input-value lambdalist)))))
		   ;; call update function on this
		   ;;(break)
		   (apply (get-rack-module-update-fn rm) (nreverse lambdalist))
		   (set-rack-module-state rm :PROCESSED-TICK)
		   ))))))
    ;; for all modules
    (dolist (rm (slot-value rack 'modules))
      (update-rm rm))
    (with-event-listeners rack :tick-fn handler
      (funcall handler))
    ))

(defun shutdown-rack (rack)
  (dolist (rm (slot-value rack 'modules))
    (funcall (get-rack-module-shutdown-fn rm)))
  (with-event-listeners rack :shutdown-fn handler
    (funcall handler)))
  
