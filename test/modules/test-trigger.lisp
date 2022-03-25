(in-package :cl-synthesizer-test)

(define-test test-trigger-1 ()
  (let ((module (cl-synthesizer-modules-trigger:make-module
		 "CVT"
		 (cl-synthesizer:make-environment)
		 :trigger-threshold 4.5
		 :pulse-voltage 2.0)))
      (assert-equality #'= 0.0 (get-module-output module :output))
      ;; Trigger on for one tick
      (update-module module (list (list :input 5.0)))
      (assert-equality #'= 2.0 (get-module-output module :output))
      (update-module module (list (list :input 5.0)))
      (assert-equality #'= 0.0 (get-module-output module :output))
      (update-module module (list (list :input 5.0)))
      (assert-equality #'= 0.0 (get-module-output module :output))
      ;; Lower input below threshold defined by switching-voltage
      (update-module module (list (list :input 2.0)))
      (assert-equality #'= 0.0 (get-module-output module :output))
      ;; Trigger on for one tick
      (update-module module (list (list :input 5.0)))
      (assert-equality #'= 2.0 (get-module-output module :output))
      (update-module module (list (list :input 5.0)))
      (assert-equality #'= 0.0 (get-module-output module :output))))


