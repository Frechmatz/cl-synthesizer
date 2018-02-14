
(defpackage :cl-synthesizer-modules-wave-file-writer
  (:use :cl)
  (:export
   :one-channel-wave-file-writer
   :two-channel-wave-file-writer))

(defpackage :cl-synthesizer-modules-vco
  (:use :cl)
  (:export
   :vco))

(defpackage :cl-synthesizer-modules-midi-interface
  (:use :cl)
  (:export
   :midi-interface))

(defpackage :cl-synthesizer-modules-multiple
  (:use :cl)
  (:export :multiple-4))

(defpackage :cl-synthesizer-modules-adsr
  (:use :cl)
  (:export :adsr))

(defpackage :cl-synthesizer-modules-vca
  (:use :cl)
  (:export :vca))

(defpackage :cl-synthesizer-modules-fixed-output
  (:use :cl)
  (:export :fixed-output))
