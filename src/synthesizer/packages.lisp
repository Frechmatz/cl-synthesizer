(defpackage :cl-synthesizer
  (:use :cl)
  (:export
   :assembly-error
   :signal-assembly-error
   :invalid-arguments-error
   :signal-invalid-arguments-error
   :make-environment
   :make-rack
   :get-line-out-adapter
   :get-midi-in-adapter
   :add-module
   :add-patch
   :update
   :shutdown
   :add-hook
   :get-environment
   :get-module
   :get-patch
   ))

