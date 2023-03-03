(defpackage :cl-synthesizer
  (:use :cl)
  (:export
   :assembly-error
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
   :is-rack
   :shutdown
   :update
   :get-modules
   :get-patches
   :expose-input-socket
   :expose-output-socket))

(defpackage :cl-synthesizer-rack-compiler
  (:use :cl)
  (:export
   :compile-rack))

