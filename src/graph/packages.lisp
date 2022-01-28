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
   :get-modules
   :get-patches
   :is-rack
   :add-module
   :get-module-name
   :get-module
   :get-environment
   :get-hooks
   :add-hook
   :add-patch
   :expose-input-socket
   :expose-output-socket
   :make-patch
   :get-patch-output-name
   :get-patch-output-socket
   :get-patch-input-name
   :get-patch-input-socket
   :get-exposed-input-socket
   :get-exposed-output-socket))

