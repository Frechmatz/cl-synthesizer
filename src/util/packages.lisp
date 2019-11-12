(defpackage :cl-synthesizer-macro-util
  (:use :cl)
  (:export
   :make-package-symbol
   :make-keyword
   :make-keyword-list
   :with-property-list
   ))

(defpackage :cl-synthesizer-lru-set
  (:use :cl)
  (:export
   :lru-set
   :remove-value
   :push-value
   :current-value
   ))
