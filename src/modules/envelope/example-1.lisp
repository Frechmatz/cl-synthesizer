(defpackage :cl-synthesizer-modules-envelope-example-1
  (:use :cl))

(in-package :cl-synthesizer-modules-envelope-example-1)

(defun example ()
  "Simple envelope example"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))

    (cl-synthesizer:add-module
     rack "ADSR"
     #'cl-synthesizer-modules-envelope:envelope
     :segments
     '(;; Attack (duration can be modulated via input socket :attack-duration)
       (:duration-ms 100 :target-cv 5 :required-gate-state :on
        :duration-controller
        (:socket :attack-duration :input-min 0.0 :input-max 5.0 :output-min 0 :output-max 800))
       ;; Decay
       (:duration-ms 50 :target-cv 3 :required-gate-state :on)
       ;; Sustain
       (:required-gate-state :on)
       ;; Release
       (:duration-ms 100 :target-cv 0 :required-gate-state :off)))

    rack))

