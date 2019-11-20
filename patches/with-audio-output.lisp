;;
;;
;; Play patches using experimental Audio output
;;
;;

(defpackage :cl-synthesizer-patches-with-audio-output
  (:use :cl))

(in-package :cl-synthesizer-patches-with-audio-output)

(asdf:load-system "cl-synthesizer-macos-device")
(asdf:load-system "cl-synthesizer-experimental")
(asdf:load-system "cl-synthesizer-patches")

(defparameter *sample-rate* 44100)  ;; cl-out123 throws division by zero on sample rates such as 22050

;;
;; Siren
;;
(defun play-siren ()
  (cl-synthesizer-experimental::play-rack
   (cl-synthesizer-patches-siren::example :sample-rate *sample-rate*)
   :duration-seconds 1.8
   :attach-audio t
   :audio-output-sockets '(:left :right)))

;;(play-siren)
