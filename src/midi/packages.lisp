
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
   :get-control-number
   :get-control-value
   ))

