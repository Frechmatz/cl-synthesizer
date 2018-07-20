(in-package :cl-synthesizer)

;;
;;
;; Rack
;;
;; Represents a grid of rack-modules
;;

(defclass rack ()
  ((modules :initform nil)
   (hooks :initform nil)
   (environment :initform nil))
  (:documentation "Represents a grid of rack-modules"))

(defmethod initialize-instance :after ((r rack) &key environment)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (if (not environment)
      (error "Environment must not be nil"))
  (setf (slot-value r 'environment) environment))

(defun get-environment (rack)
  (slot-value rack 'environment))

(defun set-state (rack state)
  (dolist (m (slot-value rack 'modules))
    (setf (slot-value m 'state) state)))

(defun get-line-out (rack)
  (slot-value (get-rm-module rack "LINE-OUT") 'module))

(defun get-midi-in (rack)
  (slot-value (get-rm-module rack "MIDI-IN") 'module))

(defun add-hook (rack hook)
  "Hook consists a property list with the following properties:
   - :update function without arguments
   - :shutdown function without arguments
   Hooks must not manipulate the rack.
   Hooks may not be called in certain situations such as when
   a rack is a embedded into another rack."
  (push hook (slot-value rack 'hooks)))

(defun create-rack (&key environment)
  (let ((rack (make-instance 'cl-synthesizer:rack :environment environment)))
    ;; Add Device Interfaces
    (add-module rack "LINE-OUT" #'cl-synthesizer:line-out)
    (add-module rack "MIDI-IN" #'cl-synthesizer:midi-in)
    rack))


;;
;; Helper classes
;;

;;
;; Rack-Module
;; 

(defclass rack-module ()
  ((state :initarg nil :documentation ":PROCESS-TICK, :PROCESSING-TICK, :TICK-PROCESSED")
   (name :initarg nil)
   (module :initarg nil)
   (input-patches :initform (make-hash-table))
   (output-patches :initform (make-hash-table))
   (input-sockets :initarg nil)
   (output-sockets :initarg nil))
  (:documentation "Represents a module holding input/output connections to other modules"))

(defmethod initialize-instance :after ((rm rack-module) &key name module)
  (setf (slot-value rm 'name) name)
  (setf (slot-value rm 'module) module)
  (let ((f (getf (slot-value rm 'module) :inputs)))
    (if f (setf (slot-value rm 'input-sockets) (funcall f))))
  (let ((f (getf (slot-value rm 'module) :outputs)))
    (if f (setf (slot-value rm 'output-sockets) (funcall f)))))

(defun get-rack-module-name (rm)
  (slot-value rm 'name))

(defun get-rack-module-state (rm)
  (slot-value rm 'state))

(defun set-rack-module-state (rm state)
  (setf (slot-value rm 'state) state))

(defun get-rack-module-input-sockets (rm)
  (slot-value rm 'input-sockets))

(defun get-rack-module-output-sockets (rm)
  (slot-value rm 'output-sockets))

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
  (:documentation ""))

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
  (find-if (lambda (rm) (string= name (get-rack-module-name rm))) (slot-value rack 'modules)))

(defun get-module (rack name)
  "Returns a module (as added via add-module) or nil"
  (let ((rm (find-if (lambda (rm) (string= name (get-rack-module-name rm))) (slot-value rack 'modules))))
    (if rm
	(slot-value rm 'module)
	nil)))

(defun add-module (rack module-name module-fn &rest args)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (if (get-rm-module rack module-name)
      (signal-assembly-error
       :format-control "A module with name ~a has already been added to the rack"
       :format-arguments (list module-name)))

  (let ((environment (slot-value rack 'environment)))
    (let ((rm (make-instance 'rack-module
			     :module (apply module-fn `(,module-name ,environment ,@args))
			     :name module-name)))
      (push rm (slot-value rack 'modules))
      rm)))

(defun add-patch (rack source-rm-name source-output-socket destination-rm-name destination-input-socket)
  (declare (optimize (debug 3) (speed 0) (space 0)))
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

(defun update-rack (rack)
  "Process a tick" 
  ;; (declare (optimize (debug 3) (speed 0) (space 0)))
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
    ;; for all hooks
    (dolist (m (slot-value rack 'hooks))
      (funcall (getf m :update)))))

(defun shutdown-rack (rack)
  (dolist (rm (slot-value rack 'modules))
    (funcall (get-rack-module-shutdown-fn rm)))
  (dolist (m (slot-value rack 'hooks))
    (if (getf m :shutdown)
	(funcall (getf m :shutdown)))))
  
;; TODO Fix inefficient implementation. Maybe rack must hold some mapping hashes.
(defun get-input-socket-patch (rack module-name input-socket)
  "Returns values (name module socket) of connected module"
  (let ((rm (get-rm-module rack module-name)))
    (if (not rm)
	nil
	(let ((patch (get-rack-module-input-patch rm input-socket)))
	  (if (not patch)
	      nil
	      (let ((patched-module-name (get-rack-patch-target-name patch))
		    (patched-rm (get-rack-patch-module patch))
		    (patched-socket (get-rack-patch-socket patch)))
		(values
		 patched-module-name
		 (get-rack-module-module patched-rm)
		 patched-socket)))))))

