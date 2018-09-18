(defpackage :cl-synthesizer
  (:use :cl)
  (:export
   :assembly-error
   :signal-assembly-error
   :invalid-arguments-error
   :signal-invalid-arguments-error
   :make-environment
   :get-environment
   :make-rack
   :add-module
   :add-patch
   :add-hook
   :get-module
   :get-patch
   :update
   :shutdown
   :get-line-out-adapter
   :get-midi-in-adapter
   :play-rack))

