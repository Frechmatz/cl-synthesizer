(in-package :cl-synthesizer)

;;
;; Bridge Modules
;;
;; The inputs and outputs of a rack are represented
;; by bridge modules INPUT and OUTPUT
;;


(defun make-input-bridge-module (input-sockets)
  "Exposes public output getters for patching. Exposes private input setters that are called by the rack." 
  (let ((values (make-array (length input-sockets) :initial-element nil))
	(outputs (make-array (length input-sockets) :initial-element nil))
	(count (length input-sockets))
	(getters nil)
	(setters nil)
	(index 0))
    (dolist (socket input-sockets)
      (let ((cur-index index))
	;; setter plist
	(push (lambda (value) (setf (elt values cur-index) value)) setters)
	(push socket setters)
	;; getter plist
	(push (lambda () (elt outputs cur-index)) getters)
	(push socket getters))
      (setf index (+ 1 index)))
    (list
     :inputs-private (lambda() setters)
     :inputs (lambda() nil) ;; no inputs that can be accessed via patching
     :outputs (lambda() getters)
     :update (lambda()
	       ;; copy values to outputs
	       (dotimes (i count)
		 (setf (elt outputs i) (elt values i)))))))

(defun make-output-bridge-module (output-sockets)
  "Exposes public input setters for patching. Exposes private output getters that are called by the rack."
  (let ((values (make-array (length output-sockets) :initial-element nil))
	(outputs (make-array (length output-sockets) :initial-element nil))
	(count (length output-sockets))
	(getters nil)
	(setters nil)
	(index 0))
    (dolist (socket output-sockets)
      (let ((cur-index index))
	;; setter plist
	(push (lambda (value) (setf (elt values cur-index) value)) setters)
	(push socket setters)
	;; getter plist
	(push (lambda () (elt outputs cur-index)) getters)
	(push socket getters))
      (setf index (+ 1 index)))
    (list
     :inputs (lambda() setters)
     :outputs-private (lambda() getters)
     :outputs (lambda() nil) ;; no outputs that can be accessed via patching
     :update (lambda()
	       ;; copy values to outputs
	       (dotimes (i count)
		 (setf (elt outputs i) (elt values i)))))))

;;
;; Rack
;;

(defun find-module (rack module-path)
  "Get a module of a rack. <p>The function has the following arguments:
    <ul>
      <li>rack The root rack.</li>
      <li>module-path The path of the module within the rack (through multiple nested racks).</br>
         Example 1: \"VCO\"</br> 
         Example 2: '(\"VOICE-1\" \"VCO\")</li>
    </ul></p>
   Returns nil or a values object consisting of the rack of the module, the module name and the module itself."
  (if (not (listp module-path))
      (setf module-path (list module-path)))
  (if (not module-path)
      (values nil nil nil)
      (let* ((module (get-module rack (first module-path))))
	(if module
	    (let ((module module))
	      (if (< 1 (length module-path))
		  (if (getf module :is-rack)
		      (find-module module (rest module-path))
		      (values nil nil nil))
		  (values rack (first module-path) module)))
	    (values nil nil nil)))))

(defun play-rack (rack &key duration-seconds)
  "A utility function that \"plays\" the rack by consecutively calling its update function
    for a given number of \"ticks\". <p>The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
	<li>:duration-seconds Duration in seconds of how long to play the rack. If for
	    example the duration is 2 seconds and the sample rate of the rack as declared
	    by its environment is 44100, then the update function of the rack will be called 88200 times.</li>
    </ul></p>"
  (let ((sample-rate (floor (getf (get-environment rack) :sample-rate)))
	(update-fn (cl-synthesizer:get-update-fn rack)))
    (dotimes (i (floor (* duration-seconds sample-rate)))
      (funcall update-fn)))
  (cl-synthesizer:shutdown rack)
  "DONE")

;;
;; The rack
;;

(defun make-rack (&key environment (input-sockets nil) (output-sockets nil))
  "Creates a rack.<p>The function has the following arguments:
    <ul>
	<li>:environment The synthesizer environment.</li>
        <li>:input-sockets The input sockets to be exposed by the rack. The inputs
        can be patched with other modules via the bridge module \"INPUT\".</li>
        <li>:output-sockets The output sockets to be exposed by the rack. The outputs
        can be patched with other modules via the bridge module \"OUTPUT\".</li>
    </ul></p>
    <p>    
    The update function of the rack calls the update function of all embedded modules. If the 
    rack has already been shut down the function immediately returns <b>nil</b>.
    Othwerwise it returns <b>t</b>.
    </p><p>
    The shutdown function of the rack calls the shutdown handlers of all embedded modules and hooks. If the rack has 
    already been shut down the function immediately returns.
    </p>"
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (if (not environment)
      (signal-invalid-arguments-error
       :format-control "Environment must not be nil"
       :format-arguments nil))

  (let ((this nil)
	(has-shut-down nil)
	;; list of (:module module :name name)
	(modules nil)
	;; List of lambda()
	(hooks nil)
	;; list of (:output-name "name" :output-socket <socket> :input-name "name" :input-socket <socket>)
	(patches nil)
	(compiled-rack nil)
	(input-bridge-module (make-input-bridge-module input-sockets))
	(output-bridge-module (make-output-bridge-module output-sockets)))
    
    (labels ((add-module (module-name module)
	       (setf compiled-rack nil)
	       (push (list :module module :name module-name) modules))
	     (add-patch (output-name output-socket input-name input-socket)
	       (setf compiled-rack nil)
	       (push (make-patch
		      :output-name output-name
		      :output-socket output-socket
		      :input-name input-name
		      :input-socket input-socket)
		     patches)))
      (let ((rack
	     (list
	      :modules (lambda() modules)
	      ;; delegate to bridge module
	      :outputs (getf output-bridge-module :outputs-private)
	      ;; delegate to bridge module
	      :inputs (getf input-bridge-module :inputs-private)
	      :patches (lambda() patches)
	      :hooks (lambda () hooks)
	      :update (lambda ()
			(if has-shut-down
			    nil
			    (progn
			      (if (not compiled-rack)
				  (setf compiled-rack (cl-synthesizer-rack-compiler:compile-rack this)))
			      (funcall compiled-rack)
			      t)))
	      :add-module (lambda (module-name module-fn &rest args)
			    (if (get-module this module-name)
				(signal-assembly-error
				 :format-control "A module with name ~a has already been added to the rack"
				 :format-arguments (list module-name)))
			    (let ((module (apply module-fn `(,module-name ,environment ,@args))))
			      (if (not (functionp (get-inputs-fn module)))
				    (signal-assembly-error
				     :format-control "Invalid module ~a: Property :input must be a function"
				     :format-arguments (list module-name)))
			      (if (not (functionp (get-outputs-fn module)))
				    (signal-assembly-error
				     :format-control "Invalid module ~a: Property :output must be a function"
				     :format-arguments (list module-name)))
			      (if (not (functionp (get-update-fn module)))
				    (signal-assembly-error
				     :format-control "Invalid module ~a: Property :update must be a function"
				     :format-arguments (list module-name)))
			      (add-module module-name module)
			      module))
	      :add-hook (lambda (hook)
			  (setf compiled-rack nil)
			  (push hook hooks))
	      :shutdown (lambda()
			  (if (not has-shut-down)
			      (progn
				(setf has-shut-down t)
				(dolist (module modules)
				  (cl-synthesizer:shutdown (getf module :module)))
				(dolist (m hooks)
				  (cl-synthesizer:shutdown m)))))
	      :environment environment
	      :is-rack t
	      :add-patch (lambda (output-name output-socket input-name input-socket)
			   (let ((source-module (get-module this output-name))
				 (destination-module (get-module this input-name)))
			     (if (not source-module)
				 (signal-assembly-error
				  :format-control "Cannot find output module ~a"
				  :format-arguments (list output-name)))
			     (if (not destination-module)
				 (signal-assembly-error
				  :format-control "Cannot find input module ~a"
				  :format-arguments (list input-name)))
			     (if (not (find output-socket (cl-synthesizer:get-outputs source-module)))
				 (signal-assembly-error
				  :format-control "Module ~a does not expose output socket ~a"
				  :format-arguments (list output-name output-socket)))
			     (if (not (find input-socket (get-inputs destination-module)))
				 (signal-assembly-error
				  :format-control "Module ~a does not expose input socket ~a"
				  :format-arguments (list input-name input-socket)))
			     (let ((p (find-if
				       (lambda (p)
					 (and (string= input-name (get-patch-input-name p))
					      (eq input-socket (get-patch-input-socket p))))
				       patches)))
			       (if p (signal-assembly-error
				      :format-control
				      "Input socket ~a of module ~a is already connected to output socket ~a of module ~a"
				      :format-arguments (list
							 input-socket
							 input-name
							 (get-patch-output-name p)
							 (get-patch-output-socket p)))))
			     (let ((p (find-if
				       (lambda (p)
					 (and (string= output-name (get-patch-output-name p))
					      (eq output-socket (get-patch-output-socket p))))
				       patches)))
			       (if p (signal-assembly-error
				      :format-control
				      "Output socket ~a of module ~a is already connected to input socket ~a of module ~a"
				      :format-arguments (list
							 output-socket
							 output-name
							 (get-patch-input-socket p)
							 (get-patch-input-name p)))))
			     (add-patch output-name output-socket input-name input-socket))))))

	(setf this rack)
	;;
	;; Add bridge modules
	;;
	(add-module "INPUT" input-bridge-module)
	(add-module "OUTPUT" output-bridge-module)

	rack))))

