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
   :get-inputs-fn
   :get-outputs-fn
   :get-update-fn
   :get-state-fn
   :get-shutdown-fn
   :get-modules-fn
   :get-patches-fn))

(defpackage :cl-synthesizer-rack-compiler
  (:use :cl)
  (:export
   :compile-rack))

