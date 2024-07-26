(in-package :cl-synthesizer-test)

(define-test test-system-1 ()
  "System module test 1"
  (let ((system (cl-synthesizer-modules-system:make-module
		"System"
		(cl-synthesizer:make-environment :sample-rate 500))))
    (update-module system nil)
    (assert-equal 0 (get-module-output system :ticks))
    (assert-equal 0.0 (get-module-output system :seconds))
    (assert-equal 0.0 (get-module-output system :milliseconds))
    (update-module system nil)
    (assert-equal 1 (get-module-output system :ticks))
    (assert-equal 1 (get-module-output system :ticks))
    (assert-equal 500.0 (get-module-output system :sample-rate))))

