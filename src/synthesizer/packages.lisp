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
   :get-module-name))

(defpackage :cl-synthesizer-rack-compiler
  (:use :cl)
  (:export
   :compile-rack))

