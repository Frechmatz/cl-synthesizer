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
   :add-hook
   :get-module
   :play-rack
   :get-module-name
   :is-rack
   :shutdown
   :update
   :get-modules
   :get-patches
   :add-rack-input
   :add-rack-output))

(defpackage :cl-synthesizer-property-list-iterator
  (:use :cl)
  (:export
   :do-property-list
   :do-property-list-keys))
