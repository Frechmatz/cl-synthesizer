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
   :get-patches
   :get-modules
   :play-rack
   :is-rack
   :find-module
   :get-module-name
   ;; Api
   :get-inputs
   :get-outputs
   :get-update-fn
   :get-state-fn
   :get-shutdown-fn))

(defpackage :cl-synthesizer-rack-compiler
  (:use :cl)
  (:export
   :compile-rack))

