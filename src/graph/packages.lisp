(defpackage :cl-synthesizer-graph
  (:use :cl)
  (:export
   :dummy
   :get-inputs-fn
   :get-inputs
   :get-outputs-fn
   :get-outputs
   :get-update-fn
   :update
   :get-state-fn
   :get-state
   :shutdown
   :get-vertices
   :get-edges
   :is-graph
   :add-vertex
   :get-vertex-name
   :get-vertex
   :get-environment
   :get-hooks
   :add-hook
   :add-edge
   :expose-input-socket
   :expose-output-socket
   :get-exposed-input-socket
   :get-exposed-output-socket))

