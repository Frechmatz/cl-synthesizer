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
   :get-modules-fn
   :get-modules
   :get-patches-fn
   :get-patches))

(defpackage :cl-synthesizer-rack-compiler
  (:use :cl)
  (:export
   :compile-rack))

