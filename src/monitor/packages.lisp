(defpackage :cl-synthesizer-monitor
  (:use :cl)
  (:export
   :add-monitor))

(defpackage :cl-synthesizer-monitor-wave-file-agent
  (:use :cl)
  (:export
   :make-backend))

(defpackage :cl-synthesizer-monitor-csv-file-agent
  (:use :cl)
  (:export
   :make-backend))

(defpackage :cl-synthesizer-monitor-buffer-agent
  (:use :cl)
  (:export
   :make-backend))
