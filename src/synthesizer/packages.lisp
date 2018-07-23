(defpackage :cl-synthesizer
  (:use :cl)
  (:export
   :assembly-error
   :signal-assembly-error
   :invalid-arguments-error
   :signal-invalid-arguments-error
   :make-environment
   :rack
   :create-rack
   :get-line-out-adapter
   :get-midi-in-adapter
   :add-module
   :add-patch
   :update-rack
   :shutdown-rack
   :add-hook
   :get-environment
   :get-module
   :get-patch
   ))

