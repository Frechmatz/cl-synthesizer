(in-package :cl-synthesizer)

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
	;; List of (:rack-socket s :module-name module :module-socket module-socket)
	(exposed-input-sockets nil)
	;; List of (:rack-socket s :module-name module :module-socket module-socket)
	(exposed-output-sockets nil)
	(exposed-inputs-dirty t)
	(exposed-outputs-dirty t)
	(rack-outputs nil)
	(rack-inputs nil))
    
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
		     patches))
	     (get-exposed-input-socket (socket)
	       (find-if
		(lambda(entry)
		  (eq socket (getf entry :rack-socket)))
		exposed-input-sockets))
	     (is-input-patched (module-name socket)
	       (find-if
		(lambda (p)
		  (and
		   (eq socket (getf p :input-socket))
		   (string= module-name (getf p :input-name))))
		patches))
	     (is-output-patched (module-name socket)
	       (find-if
		(lambda (p)
		  (and
		   (eq socket (getf p :output-socket))
		   (string= module-name (getf p :output-name))))
		patches))
	     (get-input-setter-fn (module-name socket)
	       (getf (get-inputs (get-module this module-name)) socket))
	     (get-output-getter-fn (module-name socket)
	       (getf (get-outputs (get-module this module-name)) socket))
	     (get-exposed-output-socket (socket)
	       (find-if
		(lambda(entry)
		  (eq socket (getf entry :rack-socket)))
		exposed-output-sockets))
	     (is-module-input-exposed-as-input-socket (module-name socket)
	       (find-if
		(lambda(entry)
		  (and
		   (eq socket (getf entry :module-socket))
		   (string= module-name (getf entry :module-name))))
		exposed-input-sockets))
	     (is-module-output-exposed-as-output-socket (module-name socket)
	       (find-if
		(lambda(entry)
		  (and
		   (eq socket (getf entry :module-socket))
		   (string= module-name (getf entry :module-name))))
		exposed-output-sockets))
	     (update-rack-inputs ()
	       (setf rack-inputs nil)
	       (dolist (exposed-input-socket exposed-input-sockets)
		 (setf rack-inputs
		       (push
			(get-input-setter-fn
			 (getf exposed-input-socket :module-name)
			 (getf exposed-input-socket :module-socket))
			rack-inputs))
		 (setf rack-inputs
		       (push (getf exposed-input-socket :rack-socket) rack-inputs)))
	       )
	     (update-rack-outputs ()
	       (setf rack-outputs nil)
	       (dolist (exposed-output-socket exposed-output-sockets)
		 (setf rack-outputs
		       (push
			(get-output-getter-fn
			 (getf exposed-output-socket :module-name)
			 (getf exposed-output-socket :module-socket))
			rack-outputs))
		 (setf rack-outputs
		       (push (getf exposed-output-socket :rack-socket) rack-outputs)))
	       )
	     (get-rack-inputs ()
	       (if exposed-inputs-dirty
		   (progn
		     (setf exposed-inputs-dirty nil)
		     (update-rack-inputs)))
	       rack-inputs)
	     (get-rack-outputs ()
	       (if exposed-outputs-dirty
		   (progn
		     (setf exposed-outputs-dirty nil)
		     (update-rack-outputs)))
	       rack-outputs))
      (let ((rack
	      (list
	       :get-exposed-input-socket
	       (lambda(socket)
		 (get-exposed-input-socket socket))
	       :get-exposed-output-socket
	       (lambda(socket)
		 (get-exposed-output-socket socket))
	       
	      :expose-input-socket
	      (lambda(rack-input-socket input-module-name input-socket)
		(if (get-exposed-input-socket rack-input-socket)
		    (signal-assembly-error
		     :format-control "expose-input-socket: Module already exposes input socket '~a'"
		     :format-arguments (list rack-input-socket)))
		(let ((module (get-module this input-module-name)))
		  (if (not module)
		      (signal-assembly-error
		       :format-control "expose-input-socket: Cannot find module '~a'"
		       :format-arguments (list input-module-name)))
		  (if (not (find input-socket (get-inputs module)))
		      (signal-assembly-error
		       :format-control "expose-input-socket: Module '~a' does not expose input socket '~a'"
		       :format-arguments (list input-module-name input-socket))))
		(if (is-input-patched input-module-name input-socket)
		    (signal-assembly-error
		     :format-control "expose-input-socket: Module '~a' Input Socket '~a' already patched"
		     :format-arguments (list input-module-name input-socket)))
		;;(add-patch "INPUT" rack-input-socket input-module-name input-socket)
		(setf exposed-input-sockets
		      (push (list
			     :rack-socket rack-input-socket
			     :module-name input-module-name
			     :module-socket input-socket)
			    exposed-input-sockets))
		(setf exposed-inputs-dirty t))
	      :expose-output-socket
	      (lambda(rack-output-socket output-module-name output-socket)
		(if (get-exposed-output-socket rack-output-socket)
		    (signal-assembly-error
		     :format-control "expose-output-socket: Module already exposes output socket '~a'"
		     :format-arguments (list rack-output-socket)))
		(let ((module (get-module this output-module-name)))
		  (if (not module)
		      (signal-assembly-error
		       :format-control "expose-output-socket: Cannot find module '~a'"
		       :format-arguments (list output-module-name)))
		  (if (not (find output-socket (get-outputs module)))
		      (signal-assembly-error
		       :format-control "expose-output-socket: Module '~a' does not expose output socket '~a'"
		       :format-arguments (list output-module-name output-socket))))
		(if (is-output-patched output-module-name output-socket)
		    (signal-assembly-error
		     :format-control "expose-output-socket: Module '~a' Output Socket '~a' already patched"
		     :format-arguments (list output-module-name output-socket)))

		(setf exposed-output-sockets
		      (push (list
			     :rack-socket rack-output-socket
			     :module-name output-module-name
			     :module-socket output-socket)
			    exposed-output-sockets))
		;;(add-patch output-module-name output-socket "OUTPUT" rack-output-socket)
		(setf exposed-outputs-dirty t))
	       
	      :modules (lambda() modules)
	      :outputs (lambda() (get-rack-outputs))
	      :inputs (lambda() (get-rack-inputs))
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
				 :format-control "add-module: A module with name '~a' has already been added to the rack"
				 :format-arguments (list module-name)))
			    (let ((module (apply module-fn `(,module-name ,environment ,@args))))
			      (if (not (functionp (get-inputs-fn module)))
				    (signal-assembly-error
				     :format-control "add-module: Invalid module '~a': Property :input must be a function"
				     :format-arguments (list module-name)))
			      (if (not (functionp (get-outputs-fn module)))
				    (signal-assembly-error
				     :format-control "add-module: Invalid module '~a': Property :output must be a function"
				     :format-arguments (list module-name)))
			      (if (not (functionp (get-update-fn module)))
				    (signal-assembly-error
				     :format-control "add-module: Invalid module '~a': Property :update must be a function"
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
				   :format-control "add-patch: Cannot find output module '~a'"
				   :format-arguments (list output-name)))
			      (if (not destination-module)
				  (signal-assembly-error
				   :format-control "add-patch: Cannot find input module '~a'"
				   :format-arguments (list input-name)))
			     (if (not (find output-socket (cl-synthesizer:get-outputs source-module)))
				 (signal-assembly-error
				  :format-control "add-patch: Module '~a' does not expose output socket '~a'"
				  :format-arguments (list output-name output-socket)))
			      (if (not (find input-socket (get-inputs destination-module)))
				  (signal-assembly-error
				   :format-control "add-patch: Module '~a' does not expose input socket '~a'"
				   :format-arguments (list input-name input-socket)))
			     (if (is-module-input-exposed-as-input-socket input-name input-socket)
				   (signal-assembly-error
				    :format-control
				    "add-patch: Module '~a' Input socket '~a' is exposed as rack input"
				    :format-arguments (list input-name input-socket)))
			     (if (is-module-output-exposed-as-output-socket output-name output-socket)
				   (signal-assembly-error
				    :format-control
				    "add-patch: Module '~a' Output socket '~a' is exposed as rack output"
				    :format-arguments (list input-name input-socket)))

			      (let ((p (find-if
				       (lambda (p)
					 (and (string= input-name (get-patch-input-name p))
					      (eq input-socket (get-patch-input-socket p))))
				       patches)))
			       (if p (signal-assembly-error
				      :format-control
				      "add-patch: Input socket '~a' of module '~a' is already connected with output socket '~a' of module '~a'"
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
				      "add-patch: Output socket '~a' of module '~a' is already connected with input socket '~a' of module '~a'"
				      :format-arguments (list
							 output-socket
							 output-name
							 (get-patch-input-socket p)
							 (get-patch-input-name p)))))
			     (add-patch output-name output-socket input-name input-socket))))))

	(setf this rack)
	rack))))

