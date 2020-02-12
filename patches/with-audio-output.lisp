;;
;;
;; Play patches using experimental Audio output
;;
;; For loading use slime-load-file
;;
;;

(defpackage :cl-synthesizer-patches-with-audio-output
  (:use :cl))

(in-package :cl-synthesizer-patches-with-audio-output)

(asdf:load-system "cl-synthesizer-macos-device")
(asdf:load-system "cl-synthesizer-experimental")
(asdf:load-system "cl-synthesizer-makedoc")


;;
;; Sine
;;
(defun play-sine ()
  (cl-synthesizer-experimental::play-rack
   (cl-synthesizer-patches-sine::example)
   :duration-seconds cl-synthesizer-patches-sine::*duration-seconds*
   :attach-audio t
   :audio-output-sockets cl-synthesizer-patches-sine::*audio-output-sockets*))

;;(play-sine)

;;
;; Siren
;;
(defun play-siren ()
  (cl-synthesizer-experimental::play-rack
   (cl-synthesizer-patches-siren::example)
   :duration-seconds cl-synthesizer-patches-siren::*duration-seconds*
   :attach-audio t
   :audio-output-sockets cl-synthesizer-patches-siren::*audio-output-sockets*))

;;(play-siren)


