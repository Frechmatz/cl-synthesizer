(defpackage :cl-synthesizer
  (:use :cl)
  (:export
   :signal-assembly-error
   :make-environment
   :rack
   :create-rack
   :get-line-out
   :get-midi-in
   :line-out
   :midi-in
   :get-sample-rate
   :add-module
   :add-patch
   :update-rack
   :shutdown-rack
   :get-module-input
   :get-module-output
   :get-module-input-sockets
   :add-hook
   :get-environment
   :get-module
   :get-input-socket-patch
   ))

