(defpackage :cl-synthesizer-device-speaker
  (:use :cl)
  (:export
   :mono-speaker
   :stereo-speaker))

(defpackage :cl-synthesizer-device-midi
  (:use :cl)
  (:export
   :midi-device))

(defpackage :cl-synthesizer-device-midi-sequencer
  (:use :cl)
  (:export
   :midi-sequencer))

(defpackage :cl-synthesizer-device-wave-file-writer
  (:use :cl)
  (:export
   :one-channel-wave-file-writer
   :two-channel-wave-file-writer
   :get-n-channel-wave-file-writer))




