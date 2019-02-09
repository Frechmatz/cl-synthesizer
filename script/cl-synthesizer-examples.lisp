;;
;; Runs all examples that do not require an Audio or MIDI device.
;;
;; How to run:
;;
;; (asdf:load-system :cl-synthesizer-examples)
;; (cl-synthesizer-examples::run-examples)
;;
;; See also: script/
;;
;; For now all examples must be added manually to *examples*
;;
;;

(defpackage :cl-synthesizer-examples
  (:use :cl))

(in-package :cl-synthesizer-examples)

(defparameter *examples*
  (list
   (list :name "cl-synthesizer-rack-example-1" :fn #'cl-synthesizer-rack-example-1::run-example)
   (list :name "cl-synthesizer-modules-vca-example-1" :fn #'cl-synthesizer-modules-vca-example-1::run-example)
   (list :name "cl-synthesizer-modules-vca-example-2" :fn #'cl-synthesizer-modules-vca-example-2::run-example)
   (list :name "cl-synthesizer-modules-vco-example-1" :fn #'cl-synthesizer-modules-vco-example-1::run-example)
   (list :name "cl-synthesizer-modules-vco-example-2" :fn #'cl-synthesizer-modules-vco-example-2::run-example)
   (list :name "cl-synthesizer-modules-vco-example-4" :fn #'cl-synthesizer-modules-vco-example-4::run-example)
   ;; Example is not executable because it requires Audio/MIDI devices
   ;;(list :name "cl-synthesizer-modules-midi-interface-example-1" :fn #'cl-synthesizer-modules-midi-interface-example-1::run-example)
   (list :name "cl-synthesizer-modules-midi-cc-interface-example-1" :fn #'cl-synthesizer-modules-midi-cc-interface-example-1::run-example)
   (list :name "cl-synthesizer-modules-midi-sequencer-example-1" :fn #'cl-synthesizer-modules-midi-sequencer-example-1::run-example)
   (list :name "cl-synthesizer-modules-multiple-example-1" :fn #'cl-synthesizer-modules-multiple-example-1::run-example)
   (list :name "cl-synthesizer-modules-fixed-output-example-1" :fn #'cl-synthesizer-modules-fixed-output-example-1::run-example)
   ;; Example is not executable because it is more or less a copy/paste example
   ;;(list :name "cl-synthesizer-modules-mixer-example-1" :fn #'cl-synthesizer-modules-mixer-example-1::run-example)
   (list :name "cl-synthesizer-modules-ramp-example-1" :fn #'cl-synthesizer-modules-ramp-example-1::run-example)
   (list :name "cl-synthesizer-modules-sustain-example-1" :fn #'cl-synthesizer-modules-sustain-example-1::run-example)
   (list :name "cl-synthesizer-monitor-example-1" :fn #'cl-synthesizer-monitor-example-1::run-example)
   (list :name "cl-synthesizer-modules-trigger-example-1" :fn #'cl-synthesizer-modules-trigger-example-1::run-example)
   (list :name "cl-synthesizer-modules-adsr-example-1" :fn #'cl-synthesizer-modules-adsr-example-1::run-example)
   (list :name "cl-synthesizer-modules-adsr-example-2" :fn #'cl-synthesizer-modules-adsr-example-2::run-example)
   (list :name "cl-synthesizer-modules-adsr-example-3" :fn #'cl-synthesizer-modules-adsr-example-3::run-example)))

(defun run-examples ()
  (format t "~%Running examples....")
  (finish-output)
  (dolist (example *examples*)
    (format t "~%Running example ~a..." (getf example :name))
    (finish-output)
    (funcall (getf example :fn))
    (format t "~%Example ~a has completed" (getf example :name))
    (finish-output))
  (format t "~%Examples have completed~%")
  (finish-output))

;; (run-examples)
