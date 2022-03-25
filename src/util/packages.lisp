(defpackage :cl-synthesizer-macro-util
  (:use :cl)
  (:export
   :make-package-symbol
   :make-keyword
   :make-keyword-list))

(defpackage :cl-synthesizer-lru-set
  (:use :cl)
  (:export
   :lru-set
   :remove-value
   :push-value
   :get-value
   :current-value
   :entry-count
   :empty-p))
