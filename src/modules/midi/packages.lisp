
(defpackage :cl-synthesizer-midi
  (:use :cl)
  (:export
   :get-note-number-frequency))

(defpackage :cl-synthesizer-midi-event
  (:use :cl)
  (:export
   :make-control-change-event
   :make-note-on-event
   :make-note-off-event
   :control-change-eventp
   :note-on-eventp
   :note-off-eventp
   :get-channel
   :get-note-number
   :get-velocity
   :get-controller-number
   :get-control-value))

(defpackage :cl-synthesizer-midi-lru-set
  (:use :cl))

(defpackage :cl-synthesizer-modules-midi-sequencer
  (:use :cl)
  (:export :make-module))

(defpackage :cl-synthesizer-modules-midi-polyphonic-interface
  (:use :cl)
  (:export
   :make-module))

(defpackage :cl-synthesizer-modules-midi-monophonic-interface
  (:use :cl)
  (:export
   :make-module))

(defpackage :cl-synthesizer-modules-midi-relative-cc-interface
  (:use :cl)
  (:export :make-module))
