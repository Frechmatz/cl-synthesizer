(defpackage :cl-synthesizer-monitor
  (:use :cl)
  (:export
   :add-monitor))

(defpackage :cl-synthesizer-monitor-wave-handler
  (:use :cl)
  (:export
   :wave-file-handler))

(defpackage :cl-synthesizer-monitor-wave-file-writer
  (:use :cl)
  (:export
   :wave-file-writer))

(defpackage :cl-synthesizer-monitor-csv-handler
  (:use :cl)
  (:export
   :csv-file-handler))

(defpackage :cl-synthesizer-monitor-csv-file-writer
  (:use :cl)
  (:export
   :csv-file-writer))

