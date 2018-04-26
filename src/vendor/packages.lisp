(defpackage :cl-synthesizer-vendor
  (:use :cl)
  (:export
   :get-control-number
   :get-controller-value-offset))

(defpackage :cl-synthesizer-vendor-arturia-minilab-mk2
  (:use :cl)
  (:export
   :*CONTROL-TABLE*))

(defpackage :cl-synthesizer-vendor-cc-handler
  (:use :cl)
  (:export
   :7-bit-relative))




