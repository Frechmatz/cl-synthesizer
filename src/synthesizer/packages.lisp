(defpackage :cl-synthesizer
  (:use :cl)
  (:export
   :assembly-error
   :signal-assembly-error
   :invalid-arguments-error
   :signal-invalid-arguments-error
   :make-environment
   :get-environment
   :make-device
   :make-rack
   :add-module
   :add-patch
   :add-hook
   :get-module
   :get-patch
   :play-rack
   :find-module))

