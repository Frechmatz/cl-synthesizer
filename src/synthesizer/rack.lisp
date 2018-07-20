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
  (:documentation ""))

;;
;; Helper classes
;;

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
;;
;;

(defmethod initialize-instance :after ((r rack) &key environment)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (if (not environment)
      (error "Environment must not be nil"))
  (setf (slot-value r 'environment) environment))

(defun get-environment (rack)
  (slot-value rack 'environment))

(defun assert-is-module-name-available (rack name)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (if (get-module rack name)
      (signal-assembly-error
       :format-control "A module with name ~a has already been added to the rack"
       :format-arguments (list name))))

(defun add-module (rack module-name module-fn &rest args)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (assert-is-module-name-available rack module-name)
  (let ((environment (slot-value rack 'environment)))
    (let ((rm (make-instance 'rack-module
			     :module (apply module-fn `(,module-name ,environment ,@args))
			     :name module-name)))
      (push rm (slot-value rack 'modules))
      rm)))

(defun get-module (rack name)
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
  
(defun create-rack (&key environment)
  (let ((rack (make-instance 'cl-synthesizer:rack :environment environment)))
    ;; Add Device Interfaces
    (add-module rack "LINE-OUT" #'cl-synthesizer:line-out)
    (add-module rack "MIDI-IN" #'cl-synthesizer:midi-in)
    rack))

(defun get-line-out (rack)
  (slot-value (get-module rack "LINE-OUT") 'module))

(defun get-midi-in (rack)
  (slot-value (get-module rack "MIDI-IN") 'module))

(defun add-hook (rack hook)
  "Hook consists a property list with the following properties:
   - :update function without arguments
   - :shutdown function without arguments
   Hooks must not manipulate the rack.
   Hooks may not be called in certain situations such as when
   a rack is a embedded into another rack."
  (push hook (slot-value rack 'hooks)))

;; TODO Fix inefficient implementation. Maybe rack must hold some mapping hashes.
(defun get-input-module-name (rack module-name socket)
  "Get name of module which is patched to an input socket of the given module"
  (let ((rm (get-module rack module-name)))
    (if (not rm)
	nil
	(let ((patch (get-rack-module-input-patch rm socket)))
	  (if patch
	      (get-rack-patch-target-name patch)
	      nil)))))
  
;; TODO Fix inefficient implementation. Maybe rack must hold some mapping hashes.
(defun get-module-input (rack module-name socket)
  "Get the input value of a given module and module input socket.
   - rack The rack
   - module-name Name of the module
   - socket One of the input sockets provided by the module
   Example: (get-module-input rack \"ADSR\" :gate)"
  (let ((rm (get-module rack module-name)))
    (if (not rm)
	nil
	(let ((patch (get-rack-module-input-patch rm socket)))
	  (if (not patch)
	      nil
	      (let ((source-rm (get-rack-patch-module patch))
		    (source-socket-key (get-rack-patch-socket patch)))
		(get-rack-module-output source-rm source-socket-key)))))))

;; TODO Fix inefficient implementation. Maybe rack must hold some mapping hashes.
(defun get-module-output (rack module-name socket)
  "Get the output value of a given module and socket.
   - rack The rack
   - module-name Name of the module
   - socket One of the output sockets provided by the module
   Example: (get-module-output rack \"ADSR\" :cv)"
  (let ((rm (get-module rack module-name)))
    (if (not rm)
	nil
	(get-rack-module-output rm socket))))

;; TODO Fix inefficient implementation. Maybe rack must hold some mapping hashes.
(defun get-module-output-sockets (rack module-name)
  (let ((rm (get-module rack module-name)))
    (if (not rm)
	nil
	(get-rack-module-output-sockets rm))))

;; TODO Fix inefficient implementation. Maybe rack must hold some mapping hashes.
(defun get-module-input-sockets (rack module-name)
  (let ((rm (get-module rack module-name)))
    (if (not rm)
	nil
	(get-rack-module-input-sockets rm))))
