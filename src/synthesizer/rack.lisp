(in-package :cl-synthesizer)

;;
;; Rack
;;


(defun make-patch (&key output-name output-socket input-name input-socket)
  (list
   :output-name output-name
   :output-socket output-socket
   :input-name input-name
   :input-socket input-socket))

(defun find-module (rack module-path)
  "Get a module of a rack. <p>The function has the following parameters:
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
    for a given number of \"ticks\". <p>The function has the following parameters:
    <ul>
	<li>rack The rack.</li>
	<li>:duration-seconds Duration in seconds of how long to play the rack. If for
	    example the duration is 2 seconds and the sample rate of the rack as declared
	    by its environment is 44100, then the update function of the rack will be called 88200 times.</li>
    </ul></p>"
  (let ((sample-rate (floor (getf (get-environment rack) :sample-rate)))
	(update-fn (getf rack :update)))
    (dotimes (i (floor (* duration-seconds sample-rate)))
      (funcall update-fn)))
  (cl-synthesizer:shutdown rack)
  "DONE")

(defun assert-module-structure (module-name module)
  (labels ((iterate-list (l callback)
	     (dotimes (i (/ (length l) 2))
	       (let ((element-1 (nth (* i 2) l))
		     (element-2 (nth (+ (* i 2) 1) l)))
		 (funcall callback element-1 element-2))))
	   (assert-plistp (plist format-control format-arguments)
	     (if (or (not (listp plist)) (not (evenp (length plist))))
		 (error
		  'assembly-error
		  :format-control format-control
		  :format-arguments format-arguments))
	     (iterate-list
	      plist
	      (lambda (keyword value)
		(declare (ignore value))
		(if (not (keywordp keyword))
		     (error
		      'assembly-error
		      :format-control format-control
		      :format-arguments format-arguments))))))
    (assert-plistp module "Module ~a is not a property list: ~a" (list module-name module))

    ;; Check module update function
    (if (not (functionp (getf module :update)))
	(error
	 'assembly-error
	 :format-control "Invalid module '~a': Property :update must be a function"
	 :format-arguments (list module-name)))
    ;;
    ;; Check module inputs
    ;;
    (if (not (functionp (getf module :inputs)))
	(error
	 'assembly-error
	 :format-control "Invalid module '~a': Property :inputs must be a function"
	 :format-arguments (list module-name)))
    (let ((inputs (funcall (getf module :inputs))))
      ;; check that the inputs are represented by a property list
      (assert-plistp
       inputs
       "Inputs of module ~a are not a property list: ~a"
       (list module-name inputs))
      ;; check all inputs
      (iterate-list
       inputs
       (lambda (socket socket-properties)
	 (assert-plistp
	  socket-properties
	  "Socket properties of module ~a input socket ~a are not a property list: ~a"
	  (list module-name socket socket-properties))
	 ;; check presence of setter
	 (if (not (getf socket-properties :set))
	     (error
	      'assembly-error
	      :format-control "Input socket ~a of module ~a does not have a 'set' function"
	      :format-arguments (list socket module-name)))
	 ;; check if setter is a function
	 (if (not (functionp (getf socket-properties :set)))
	     (error
	      'assembly-error
	      :format-control "Setter of input socket ~a of module ~a is not a function"
	      :format-arguments (list socket module-name)))
	 ;; check presence of getter
	 (if (not (getf socket-properties :get))
	     (error
	      'assembly-error
	      :format-control "Input socket ~a of module ~a does not have a 'get' function"
	      :format-arguments (list socket module-name)))
	 ;; check if getter is a function
	 (if (not (functionp (getf socket-properties :get)))
	     (error
	      'assembly-error
	      :format-control "Getter of input socket ~a of module ~a is not a function"
	      :format-arguments (list socket module-name))))))
    ;;
    ;; Check module outputs
    ;;
    (if (not (functionp (getf module :outputs)))
	(error
	 'assembly-error
	 :format-control "Invalid module '~a': Property :outputs must be a function"
	 :format-arguments (list module-name)))
    (let ((outputs (funcall (getf module :outputs))))
      ;; check that the outputs are represented by a property list
      (assert-plistp
       outputs
       "Outputs of module ~a are not a property list: ~a"
       (list module-name outputs))
      ;; check all outputs
      (iterate-list
       outputs
       (lambda (socket socket-properties)
	 (assert-plistp
	  socket-properties
	  "Socket properties of module ~a output socket ~a are not a property list: ~a"
	  (list module-name socket socket-properties))
	 ;; check presence of getter
	 (if (not (getf socket-properties :get))
	     (error
	      'assembly-error
	      :format-control "Output socket ~a of module ~a does not have a 'get' function"
	      :format-arguments (list socket module-name)))
	 ;; check if getter is a function
	 (if (not (functionp (getf socket-properties :get)))
	     (error
	      'assembly-error
	      :format-control "Getter of output socket ~a of module ~a is not a function"
	      :format-arguments (list socket module-name))))))))
	  
(defun assert-input-socket-p (module socket)
  (let ((inputs (funcall (getf module :inputs))))
    (if (not (getf inputs socket))
	(error
	 'simple-error
	 :format-control
	 "Internal Error: Input socket ~a should be exposed by a module but is not. Inputs: ~a"
	 :format-arguments (list socket inputs)))))

	
(defun assert-output-socket-p (module socket)
  (let ((outputs (funcall (getf module :outputs))))
    (if (not (getf outputs socket))
	(error
	 'simple-error
	 :format-control
	 "Internal Error: Output socket ~a should be exposed by a module but is not. Outputs: ~a"
	 :format-arguments (list socket outputs)))))

;;
;; The rack
;;

(defun make-rack (&key environment)
  "Creates a rack.<p>The function has the following parameters:
    <ul>
	<li>:environment The synthesizer environment.</li>
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
      (error
       'simple-error
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
	       (getf (getf (funcall (getf (get-module this module-name) :inputs)) socket) :set))
	     (get-input-getter-fn (module-name socket)
	       (getf (getf (funcall (getf (get-module this module-name) :inputs)) socket) :get))
	     (get-output-getter-fn (module-name socket)
	       (getf (getf (funcall (getf (get-module this module-name) :outputs)) socket) :get))
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
		       (push (list
			      :set (get-input-setter-fn
				    (getf exposed-input-socket :module-name)
				    (getf exposed-input-socket :module-socket))
			      :get (get-input-getter-fn
				    (getf exposed-input-socket :module-name)
				    (getf exposed-input-socket :module-socket)))
			     rack-inputs))
		 (setf rack-inputs
		       (push (getf exposed-input-socket :rack-socket) rack-inputs))))
	     (update-rack-outputs ()
	       (let ((new-outputs nil))
		 (dolist (exposed-output-socket exposed-output-sockets)
		   (push
		    (list :get (get-output-getter-fn
				(getf exposed-output-socket :module-name)
				(getf exposed-output-socket :module-socket)))
		    new-outputs)
		   (push (getf exposed-output-socket :rack-socket) new-outputs))
		 (setf rack-outputs new-outputs))))
      (let ((rack
	      (list
	       :get-exposed-input-socket
	       (lambda(socket)
		 (get-exposed-input-socket socket))
	       :get-exposed-input-sockets
	       (lambda()
		 exposed-input-sockets)
	       :get-exposed-output-socket
	       (lambda(socket)
		 (get-exposed-output-socket socket))
	       :get-exposed-output-sockets
	       (lambda()
		 exposed-output-sockets)
	       :expose-input-socket
	       (lambda(rack-input-socket input-module-name input-socket)
		 (if (get-exposed-input-socket rack-input-socket)
		     (error
		      'assembly-error
		      :format-control
		      "expose-input-socket: Module already exposes input socket '~a'"
		      :format-arguments (list rack-input-socket)))
		 (let ((module (get-module this input-module-name)))
		   (if (not module)
		       (error
			'assembly-error
			:format-control
			"expose-input-socket: Cannot find module '~a'"
			:format-arguments (list input-module-name)))
		   (if (not (find input-socket (funcall (getf module :inputs))))
		       (error
			'assembly-error
			:format-control
			"expose-input-socket: Module '~a' does not expose input socket '~a'"
			:format-arguments (list input-module-name input-socket)))
		   (if (is-input-patched input-module-name input-socket)
		       (error
			'assembly-error
			:format-control
			"expose-input-socket: Module '~a' Input Socket '~a' already patched"
			:format-arguments (list input-module-name input-socket)))
		  
		   (setf exposed-input-sockets
			 (push (list
				:rack-socket rack-input-socket
				:module-name input-module-name
				:module-socket input-socket)
			       exposed-input-sockets))
		   (update-rack-inputs)
		   (assert-input-socket-p this rack-input-socket)))
	       :expose-output-socket
	       (lambda(rack-output-socket output-module-name output-socket)
		 (if (get-exposed-output-socket rack-output-socket)
		     (error
		      'assembly-error
		      :format-control
		      "expose-output-socket: Module already exposes output socket '~a'"
		      :format-arguments (list rack-output-socket)))
		 (let ((module (get-module this output-module-name)))
		   (if (not module)
		       (error
			'assembly-error
			:format-control
			"expose-output-socket: Cannot find module '~a'"
			:format-arguments (list output-module-name)))
		   (if (not (find output-socket (funcall (getf module :outputs))))
		       (error
			'assembly-error
			:format-control
			"expose-output-socket: Module '~a' does not expose output socket '~a'"
			:format-arguments (list output-module-name output-socket)))
		   (if (is-output-patched output-module-name output-socket)
		       (error
			'assembly-error
			:format-control
			"expose-output-socket: Module '~a' Output Socket '~a' already patched"
			:format-arguments (list output-module-name output-socket)))
			 (push (list
				:rack-socket rack-output-socket
				:module-name output-module-name
				:module-socket output-socket)
			       exposed-output-sockets)
		   (update-rack-outputs)
		   (assert-output-socket-p this rack-output-socket)))
	       :modules (lambda() modules)
	       :outputs (lambda() rack-outputs)
	       :inputs (lambda() rack-inputs)
	       :patches (lambda() patches)
	       :hooks (lambda () hooks)
	       :update (lambda ()
			 (if has-shut-down
			     nil
			     (progn
			       (if (not compiled-rack)
				   (setf
				    compiled-rack
				    (cl-synthesizer-rack-compiler:compile-rack this)))
			       (funcall compiled-rack)
			       t)))
	       :add-module (lambda (module-name module-fn &rest args)
			     (if (get-module this module-name)
				 (error
				  'assembly-error
				  :format-control
				  "add-module: A module with name '~a' has already been added to the rack"
				  :format-arguments
				  (list module-name)))
			     ;; Instantiate Module
			     (let ((module (apply module-fn `(,module-name ,environment ,@args))))
			       (assert-module-structure module-name module)
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
				  (error
				   'assembly-error
				   :format-control "add-patch: Cannot find output module '~a'"
				   :format-arguments (list output-name)))
			      (if (not destination-module)
				  (error
				   'assembly-error
				   :format-control "add-patch: Cannot find input module '~a'"
				   :format-arguments (list input-name)))
			      (if (not (find output-socket (funcall (getf source-module :outputs))))
				  (error
				   'assembly-error
				   :format-control "add-patch: Module '~a' does not expose output socket '~a'"
				   :format-arguments (list output-name output-socket)))
			      (if (not (find input-socket (funcall (getf destination-module :inputs))))
				  (error
				   'assembly-error
				   :format-control "add-patch: Module '~a' does not expose input socket '~a'"
				   :format-arguments (list input-name input-socket)))
			      (if (is-module-input-exposed-as-input-socket input-name input-socket)
				  (error
				   'assembly-error
				   :format-control
				   "add-patch: Module '~a' Input socket '~a' is exposed as rack input"
				   :format-arguments (list input-name input-socket)))
			      (if (is-module-output-exposed-as-output-socket output-name output-socket)
				  (error
				   'assembly-error
				   :format-control
				   "add-patch: Module '~a' Output socket '~a' is exposed as rack output"
				   :format-arguments (list input-name input-socket)))

			      (let ((p (find-if
					(lambda (p)
					  (and (string= input-name (getf p :input-name))
					       (eq input-socket (getf p :input-socket))))
					patches)))
				(if p (error
				       'assembly-error
				       :format-control
				       "add-patch: Input socket '~a' of module '~a' is already connected with output socket '~a' of module '~a'"
				       :format-arguments (list
							  input-socket
							  input-name
							  (getf p :output-name)
							  (getf p :output-socket)))))
			      (let ((p (find-if
					(lambda (p)
					  (and (string= output-name (getf p :output-name))
					       (eq output-socket (getf p :output-socket))))
					patches)))
				(if p (error
				       'assembly-error
				       :format-control
				       "add-patch: Output socket '~a' of module '~a' is already connected with input socket '~a' of module '~a'"
				       :format-arguments (list
							  output-socket
							  output-name
							  (getf p :input-socket)
							  (getf p :input-name)))))
			      (add-patch output-name output-socket input-name input-socket))))))

	(setf this rack)
	rack))))

