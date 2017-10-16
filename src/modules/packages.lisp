
(defpackage :cl-synthesizer-modules
  (:use :cl)
)

(defpackage :cl-synthesizer-modules-sinus-vco
  (:use :cl)
  (:export
   :sinus-vco))

(defpackage :cl-synthesizer-modules-wave-file-writer
  (:use :cl)
  (:export
   :one-channel-wave-file-writer
   :two-channel-wave-file-writer))


