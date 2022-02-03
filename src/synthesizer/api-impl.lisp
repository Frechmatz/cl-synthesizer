;;
;; Preliminary implementation of graph interface for
;; cl-synthesizer based on property list approach
;;

(in-package :cl-synthesizer)

(defmethod cl-synthesizer-graph:get-inputs-fn (module)
  (getf module :inputs))

(defmethod cl-synthesizer-graph:get-inputs (module)
  (funcall (getf module :inputs)))

(defmethod cl-synthesizer-graph:get-outputs-fn (module)
  (getf module :outputs))

(defmethod cl-synthesizer-graph:get-outputs (module)
  (funcall (getf module :outputs)))

(defmethod cl-synthesizer-graph:get-update-fn (module)
  (getf module :update))

(defmethod cl-synthesizer-graph:update (module)
  (funcall (getf module :update)))

(defmethod cl-synthesizer-graph:get-state-fn (module)
  (getf module :state))

(defmethod cl-synthesizer-graph:get-state (module key)
  (let ((fn (get-state-fn module)))
    (if fn
	(funcall fn key)
	nil)))

(defmethod cl-synthesizer-graph:shutdown (module)
  (let ((fn (getf module :shutdown)))
    (if fn (funcall fn))))

(defmethod cl-synthesizer-graph:get-modules (rack)
  (funcall (getf rack :modules)))

(defmethod cl-synthesizer-graph:get-patches (rack)
  (funcall (getf rack :patches)))

(defmethod cl-synthesizer-graph:is-rack (module)
  (getf module :is-rack))

(defmethod cl-synthesizer-graph:add-module
    (rack
     module-name module-fn
     &rest args)
  (apply (getf rack :add-module) module-name module-fn args))

(defmethod cl-synthesizer-graph:get-module-name (rack module)
  (let ((match
	    (find-if
	     (lambda (cur-module) (eq module (getf cur-module :module)))
	     (get-modules rack))))
    (if match (getf match :name) nil)))

(defmethod cl-synthesizer-graph:get-module (rack name)
  (let ((module
	 (find-if
	  (lambda (m) (string= name (getf m :name)))
	  (get-modules rack))))
    (if module (getf module :module) nil)))

(defmethod cl-synthesizer-graph:get-environment (rack)
  (getf rack :environment))

(defmethod cl-synthesizer-graph:get-hooks (rack)
  (funcall (getf rack :hooks)))

(defmethod cl-synthesizer-graph:add-hook (rack hook)
  (funcall (getf rack :add-hook) hook))

(defmethod cl-synthesizer-graph:add-patch
    (rack
     output-module-name
     output-socket
     input-module-name
     input-socket)
  (funcall
   (getf rack :add-patch)
   output-module-name
   output-socket
   input-module-name
   input-socket))

(defmethod cl-synthesizer-graph:expose-input-socket
    (rack
     rack-input-socket
     input-module-name
     input-socket)
  (funcall
   (getf rack :expose-input-socket)
   rack-input-socket
   input-module-name
   input-socket))

(defmethod cl-synthesizer-graph:expose-output-socket
    (rack
     rack-output-socket
     output-module-name
     output-socket)
  (funcall
   (getf rack :expose-output-socket)
   rack-output-socket
   output-module-name
   output-socket))

(defmethod cl-synthesizer-graph:make-patch
    (&key output-name
       output-socket
       input-name
       input-socket)
  (list
   :output-name output-name
   :output-socket output-socket
   :input-name input-name
   :input-socket input-socket))

(defmethod cl-synthesizer-graph:get-patch-output-name (patch)
  (getf patch :output-name))

(defmethod cl-synthesizer-graph:get-patch-output-socket (patch)
  (getf patch :output-socket))

(defmethod cl-synthesizer-graph:get-patch-input-name (patch)
  (getf patch :input-name))

(defmethod cl-synthesizer-graph:get-patch-input-socket (patch)
  (getf patch :input-socket))

(defmethod cl-synthesizer-graph:get-exposed-input-socket (rack socket)
  (funcall (getf rack :get-exposed-input-socket) socket))

(defmethod cl-synthesizer-graph:get-exposed-output-socket (rack socket)
  (funcall (getf rack :get-exposed-output-socket) socket))

