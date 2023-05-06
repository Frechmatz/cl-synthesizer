(defpackage :cl-synthesizer-profiling-compiler
  (:use :cl))

(in-package :cl-synthesizer-profiling-compiler)


(defun make-test-rack ()
  (cl-synthesizer-profiling-keyboard::make-test-rack
   :duration-seconds 10
   :voice-count 50
   :exponential nil))

