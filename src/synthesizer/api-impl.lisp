;;
;; Preliminary implementation of graph interface for
;; cl-synthesizer based on property list approach
;;

(in-package :cl-synthesizer)

(defmethod cl-synthesizer-graph:get-inputs-fn (vertex)
  (getf vertex :inputs))

(defmethod cl-synthesizer-graph:get-inputs (vertex)
  (funcall (getf vertex :inputs)))

(defmethod cl-synthesizer-graph:get-outputs-fn (vertex)
  (getf vertex :outputs))

(defmethod cl-synthesizer-graph:get-outputs (vertex)
  (funcall (getf vertex :outputs)))

(defmethod cl-synthesizer-graph:get-update-fn (vertex)
  (getf vertex :update))

(defmethod cl-synthesizer-graph:update (vertex)
  (funcall (getf vertex :update)))

(defmethod cl-synthesizer-graph:get-state-fn (vertex)
  (getf vertex :state))

(defmethod cl-synthesizer-graph:get-state (vertex key)
  (let ((fn (get-state-fn vertex)))
    (if fn
	(funcall fn key)
	nil)))

(defmethod cl-synthesizer-graph:shutdown (vertex)
  (let ((fn (getf vertex :shutdown)))
    (if fn (funcall fn))))

(defmethod cl-synthesizer-graph:get-vertices (graph)
  (funcall (getf graph :modules)))

(defmethod cl-synthesizer-graph:get-edges (graph)
  (funcall (getf graph :patches)))

(defmethod cl-synthesizer-graph:is-graph (vertex)
  (getf vertex :is-rack))

(defmethod cl-synthesizer-graph:add-vertex
    (graph
     vertex-name vertex-ctor-fn
     &rest args)
  (apply (getf graph :add-module) vertex-name vertex-ctor-fn args))

(defmethod cl-synthesizer-graph:get-vertex-name (graph vertex)
  (let ((match
	    (find-if
	     (lambda (cur-module) (eq vertex (getf cur-module :module)))
	     (cl-synthesizer:get-modules graph))))
    (if match (getf match :name) nil)))

(defmethod cl-synthesizer-graph:get-vertex (graph vertex-name)
  (let ((module
	 (find-if
	  (lambda (m) (string= vertex-name (getf m :name)))
	  (cl-synthesizer:get-modules graph))))
    (if module (getf module :module) nil)))

(defmethod cl-synthesizer-graph:get-environment (graph)
  (getf graph :environment))

(defmethod cl-synthesizer-graph:get-hooks (graph)
  (funcall (getf graph :hooks)))

(defmethod cl-synthesizer-graph:add-hook (graph hook)
  (funcall (getf graph :add-hook) hook))

(defmethod cl-synthesizer-graph:add-edge
    (graph
     output-vertex-name
     output-socket
     input-vertex-name
     input-socket)
  (funcall
   (getf graph :add-patch)
   output-vertex-name
   output-socket
   input-vertex-name
   input-socket))

(defmethod cl-synthesizer-graph:expose-input-socket
    (graph
     graph-input-socket
     input-vertex-name
     input-socket)
  (funcall
   (getf graph :expose-input-socket)
   graph-input-socket
   input-vertex-name
   input-socket))

(defmethod cl-synthesizer-graph:expose-output-socket
    (graph
     graph-output-socket
     output-vertex-name
     output-socket)
  (funcall
   (getf graph :expose-output-socket)
   graph-output-socket
   output-vertex-name
   output-socket))

(defmethod cl-synthesizer-graph:make-edge
    (&key output-vertex-name
       output-socket
       input-vertex-name
       input-socket)
  (list
   :output-name output-vertex-name
   :output-socket output-socket
   :input-name input-vertex-name
   :input-socket input-socket))

(defmethod cl-synthesizer-graph:get-edge-output-name (edge)
  (getf edge :output-name))

(defmethod cl-synthesizer-graph:get-edge-output-socket (edge)
  (getf edge :output-socket))

(defmethod cl-synthesizer-graph:get-edge-input-name (edge)
  (getf edge :input-name))

(defmethod cl-synthesizer-graph:get-edge-input-socket (edge)
  (getf edge :input-socket))

(defmethod cl-synthesizer-graph:get-exposed-input-socket (graph socket)
  (funcall (getf graph :get-exposed-input-socket) socket))

(defmethod cl-synthesizer-graph:get-exposed-output-socket (graph socket)
  (funcall (getf graph :get-exposed-output-socket) socket))

