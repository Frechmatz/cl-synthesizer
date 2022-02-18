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
(defgeneric get-vertices (graph callback-fn)
  (:documentation "Callback: (lambda (vertex-name))"))
(defgeneric get-edges (graph edge-callback-fn)
  (:documentation "Callback: (lambda (output-name output-socket input-name input-socket))"))
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
(defgeneric get-exposed-input-socket (graph socket))
(defgeneric get-exposed-output-socket (graph socket))
