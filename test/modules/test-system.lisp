(in-package :cl-synthesizer-test)

(define-test test-system-1 ()
  "System module test 1"
  (let ((system (cl-synthesizer-modules-system:make-module
		"System"
		(cl-synthesizer:make-environment :sample-rate 500))))
    (assert-equal 0 (get-module-state system :elapsed-ticks))
    (assert-equal 0.0 (get-module-state system :elapsed-seconds))
    (assert-equal 0.0 (get-module-state system :elapsed-milliseconds))
    (update-module system nil)
    (assert-equal 1 (get-module-state system :elapsed-ticks))
    (assert-equal 0.002 (get-module-state system :elapsed-seconds))
    (assert-equal 2.0 (get-module-state system :elapsed-milliseconds))
    (assert-equal 500.0 (get-module-state system :sample-rate))))

