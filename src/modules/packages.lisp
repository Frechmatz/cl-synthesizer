
(defpackage :cl-synthesizer-modules-vco
  (:use :cl)
  (:export
   :vco-base
   :vco-exponential
   :vco-linear))

(defpackage :cl-synthesizer-modules-midi-interface
  (:use :cl)
  (:export
   :midi-interface))

(defpackage :cl-synthesizer-modules-multiple
  (:use :cl)
  (:export :multiple))

(defpackage :cl-synthesizer-modules-envelope
  (:use :cl)
  (:export
   :envelope))

(defpackage :cl-synthesizer-modules-vca
  (:use :cl)
  (:export
   :vca))

(defpackage :cl-synthesizer-modules-fixed-output
  (:use :cl)
  (:export :fixed-output))

(defpackage ::cl-synthesizer-modules-midi-sequencer
  (:use :cl)
  (:export :midi-sequencer))

