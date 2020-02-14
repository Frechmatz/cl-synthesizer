(defpackage :cl-synthesizer-monitor
  (:use :cl)
  (:export
   :add-monitor)
  (:documentation "A monitor is a high-level Rack-Hook. A Rack-Hook is a function
    that is called after each \"tick\" of the synthesizer. Monitors provide 
    a simple syntax to access the values of input sockets, output sockets
    and state sockets. These values are passed to a so called Monitor-Handler.
    The cl-synthesizer library provides Monitor-Handlers for writing Wave and CSV files."))

(defpackage :cl-synthesizer-monitor-wave-handler
  (:use :cl)
  (:export
   :make-handler))

(defpackage :cl-synthesizer-monitor-csv-handler
  (:use :cl)
  (:export
   :make-handler))

