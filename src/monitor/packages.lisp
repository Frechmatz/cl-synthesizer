(defpackage :cl-synthesizer-monitor
  (:use :cl)
  (:export
   :add-monitor))

(defpackage :cl-synthesizer-monitor-wave-handler
  (:use :cl)
  (:export
   :make-backend))

(defpackage :cl-synthesizer-monitor-csv-handler
  (:use :cl)
  (:export
   :make-backend))

