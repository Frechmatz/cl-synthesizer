
(defpackage :cl-synthesizer-modules-base-vco
  (:use :cl))

(defpackage :cl-synthesizer-modules-linear-vco
  (:use :cl)
  (:export :make-module))

(defpackage :cl-synthesizer-modules-exponential-vco
  (:use :cl)
  (:export :make-module))

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
