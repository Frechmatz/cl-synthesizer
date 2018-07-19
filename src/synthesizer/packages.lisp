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
   :register-monitor
   :add-module
   :add-patch
   :update-rack
   :shutdown-rack
   :get-module-input
   :get-module-output
   :get-input-module-name
   :get-module-output-sockets
   :get-module-input-sockets
   ))

