
(defpackage :cl-synthesizer-modules-vco
  (:use :cl)
  (:export
   :make-module-base
   :make-exponential-module
   :make-linear-module))

(defpackage :cl-synthesizer-modules-midi-interface
  (:use :cl)
  (:export
   :make-module))

(defpackage ::cl-synthesizer-modules-midi-cc-interface
  (:use :cl)
  (:export :make-module))

(defpackage :cl-synthesizer-modules-multiple
  (:use :cl)
  (:export :make-module))

(defpackage :cl-synthesizer-modules-envelope
  (:use :cl)
  (:export
   :make-module))

(defpackage :cl-synthesizer-modules-vca
  (:use :cl)
  (:export
   :make-module))

(defpackage :cl-synthesizer-modules-fixed-output
  (:use :cl)
  (:export :make-module))

(defpackage ::cl-synthesizer-modules-midi-sequencer
  (:use :cl)
  (:export :make-module))

(defpackage :cl-synthesizer-modules-adder
  (:use :cl)
  (:export :make-module))

(defpackage :cl-synthesizer-modules-mixer
  (:use :cl)
  (:export :make-module))
