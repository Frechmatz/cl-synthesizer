
(in-package :cl-synthesizer)

(define-condition assembly-error (error) ())

(defun signal-assembly-error (&key format-control format-arguments)
  ;;(break)
  (error (make-condition
	  'assembly-error
	  :format-control format-control
	  :format-arguments format-arguments)))

(defparameter *ENVIRONMENT* (list :sample-rate 44100))


;;
;; Todos:
;; - a shutdown concept (specific problem is streaming into wave files)
;;

;;
;; Represents a module holding input/output connections to other modules
;;
(defclass rack-module ()
  ((state :initarg nil :documentation ":PROCESS-TICK, :PROCESSING-TICK, :TICK-PROCESSED")
   (name :initarg nil)
   (module :initarg nil)
   (input-patches :initform (make-hash-table))
   (output-patches :initform (make-hash-table))))

(defun make-rack-module (name module-fn &rest args)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((rm (make-instance 'rack-module)) (m (apply module-fn `(,*ENVIRONMENT* ,@args))))
    (setf (slot-value rm 'name) name)
    (setf (slot-value rm 'module) m)
    rm))

(defun get-rack-module-name (rm)
  (slot-value rm 'name))

(defun get-rack-module-state (rm)
  (slot-value rm 'state))

(defun set-rack-module-state (rm state)
  (setf (slot-value rm 'state) state))

(defun get-rack-module-input-sockets (rm)
  (let ((f (getf (slot-value rm 'module) :inputs)))
    (if f (funcall f) nil)))

(defun get-rack-module-output-sockets (rm)
  (let ((f (getf (slot-value rm 'module) :outputs)))
    (if f (funcall f) nil)))

(defun get-rack-module-update-fn (rm)
  (getf (slot-value rm 'module) :update))

(defun get-rack-module-output-fn (rm)
  (getf (slot-value rm 'module) :get-output))

(defun get-rack-module-shutdown-fn (rm)
  (let ((f (getf (slot-value rm 'module) :shutdown)))
    (if f f (lambda() ()))))

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

(defun get-rack-module-input-patch (rm input-socket)
  (gethash input-socket (slot-value rm 'input-patches)))

(defun get-rack-module-output-patch (rm output-socket)
  (gethash output-socket (slot-value rm 'output-patches)))

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

;;
;; Represents a grid of rack-modules
;;
(defclass rack ()
  ((modules :initform nil)))

(defun get-module (rack name)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (find-if (lambda (rm) (string= name (get-rack-module-name rm))) (slot-value rack 'modules)))

(defun assert-is-module-name-available (rack name)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (if (get-module rack name)
      (signal-assembly-error
       :format-control "Module name ~a is not available"
       :format-arguments (list name))))

(defun add-module (rack rack-module)
  (assert-is-module-name-available rack (get-rack-module-name rack-module))
  (push rack-module (slot-value rack 'modules)))

(defun set-state (rack state)
  (dolist (m (slot-value rack 'modules))
    (setf (slot-value m 'state) state)))

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
      (update-rm rm))))

(defun shutdown-rack (rack)
  (dolist (rm (slot-value rack 'modules))
    (funcall (get-rack-module-shutdown-fn rm))))
  
