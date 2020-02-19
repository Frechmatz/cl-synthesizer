(defpackage :cl-synthesizer-monitor
  (:use :cl)
  (:export
   :add-monitor)
  (:documentation "A monitor is a high-level Rack-Hook. The purpose of a monitor
    is to collect module states and pass them to a \"Monitor-Backend\". A backend may
    for example generate a Wave file. 
    Monitors provide a simple syntax for declaring the module sockets to be tracked (input, output and state)
    as well as any other settings supported by specific backends.</br>
    A backend is typically realized as a plain module, which declares
    input sockets, output sockets, a shutdown function and so on. Backends are 
    instantiated by a so called \"Monitor-Handler\".</br>
    A Monitor-Handler is a factory function whose purpose is to prepare the initialization 
    parameters of a specific backend (e.g. a Wave-File-Writer module), to set up the 
    mapping of the values collected by the monitor to input sockets of the backend 
    and finally to instantiate it.</br>
    cl-synthesizer comes with handlers for the writing of Wave and CSV files."))

(defpackage :cl-synthesizer-monitor-wave-handler
  (:use :cl)
  (:export
   :make-backend))

(defpackage :cl-synthesizer-monitor-csv-handler
  (:use :cl)
  (:export
   :make-backend))

