(defpackage :cl-synthesizer-doc-util
  (:use :cl)
  (:export
   :example-code
   :make-function-string
   :make-condition-string
   :read-text-file
   :current-date
   :make-path))


(defpackage :cl-synthesizer-makedoc
  (:use :cl :cl-synthesizer-doc-util))
