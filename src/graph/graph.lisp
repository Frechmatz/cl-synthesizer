;;
;; Graph Interface
;; Work in progress
;;

(in-package :cl-synthesizer-graph)

(defgeneric get-inputs-fn (vertex))
(defgeneric get-inputs (vertex))
(defgeneric get-outputs-fn (vertex))
(defgeneric get-outputs (vertex))
(defgeneric get-update-fn (vertex))
(defgeneric update (vertex))
(defgeneric get-state-fn (vertex))
(defgeneric get-state (vertex key))
(defgeneric shutdown (vertex))
(defgeneric get-vertices (graph))
(defgeneric get-edges (graph)
  (:documentation
   "Returns list of (&key output-vertex-name output-socket input-vertex-name input-socket)"))
(defgeneric is-graph (vertex))
(defgeneric add-vertex (graph vertex-name vertex-ctor-fn &rest args))
(defgeneric get-vertex-name (graph vertex))
(defgeneric get-vertex (graph vertex-name))
(defgeneric get-environment (graph))
(defgeneric get-hooks (graph))
(defgeneric add-hook (graph hook))
(defgeneric add-edge (graph output-vertex-name output-socket input-vertex-name input-socket))
(defgeneric expose-input-socket (graph graph-input-socket input-vertex-name input-socket))
(defgeneric expose-output-socket (graph graph-output-socket output-vertex-name output-socket))
(defgeneric make-edge (&key output-vertex-name output-socket input-vertex-name input-socket))
(defgeneric get-edge-output-name (edge))
(defgeneric get-edge-output-socket (edge))
(defgeneric get-edge-input-name (edge))
(defgeneric get-edge-input-socket (edge))
(defgeneric get-exposed-input-socket (graph socket))
(defgeneric get-exposed-output-socket (graph socket))
