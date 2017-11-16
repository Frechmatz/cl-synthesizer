
(defpackage :cl-synthesizer-modules-constants
  (:use :cl)
  (:export
   :+V-PEAK+))

(defpackage :cl-synthesizer-modules-macro-util
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

(defpackage :cl-synthesizer-modules-speaker
  (:use :cl)
  (:export
   :mono-speaker
   :stereo-speaker))

(defpackage :cl-synthesizer-modules-vco
  (:use :cl)
  (:export
   :vco))

(defpackage :cl-synthesizer-modules-step-sequencer
  (:use :cl)
  (:export
   :step-sequencer))
