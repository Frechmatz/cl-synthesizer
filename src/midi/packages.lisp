
(defpackage :cl-synthesizer-midi
  (:use :cl)
  (:export
   :get-note-number-frequency
   :relative-cc-handler))

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
   :get-controller-value))

(defpackage :cl-synthesizer-midi-voice-manager
  (:use :cl)
  (:export
   :voice-manager
   :push-note
   :remove-note))

