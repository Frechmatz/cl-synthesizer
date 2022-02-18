(defpackage :cl-synthesizer
  (:use :cl)
  (:export
   :assembly-error
   :signal-assembly-error
   :invalid-arguments-error
   :signal-invalid-arguments-error
   :*home-directory*
   :make-environment
   :get-environment
   :make-rack
   :add-module
   :add-patch
   :get-hooks
   :add-hook
   :get-module
   :play-rack
   :find-module
   :get-module-name
   ;; Api
   :is-rack
   :shutdown
   :get-inputs-fn
   :get-inputs
   :get-outputs-fn
   :get-outputs
   :get-update-fn
   :update
   :get-state-fn
   :get-state
   :get-modules
   :get-patches
   :get-patch-output-name
   :get-patch-output-socket
   :get-patch-input-name
   :get-patch-input-socket
   :expose-input-socket
   :expose-output-socket))

(defpackage :cl-synthesizer-rack-compiler
  (:use :cl)
  (:export
   :compile-rack))

