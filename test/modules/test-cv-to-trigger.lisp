(in-package :cl-synthesizer-test)

(define-test test-cv-to-trigger-1 ()
  (let ((module (cl-synthesizer-modules-cv-to-trigger:make-module
		 "CVT"
		 (cl-synthesizer:make-environment)
		 :trigger-threshold 4.5
		 :pulse-voltage 2.0)))
      (assert-equality #'= 0.0 (funcall (getf module :get-output) :output))
      ;; Trigger on for one tick
      (funcall (getf module :update) :input 5.0)
      (assert-equality #'= 2.0 (funcall (getf module :get-output) :output))
      (funcall (getf module :update) :input 5.0)
      (assert-equality #'= 0.0 (funcall (getf module :get-output) :output))
      (funcall (getf module :update) :input 5.0)
      (assert-equality #'= 0.0 (funcall (getf module :get-output) :output))
      ;; Lower input below threshold defined by switching-voltage
      (funcall (getf module :update) :input 2.0)
      (assert-equality #'= 0.0 (funcall (getf module :get-output) :output))
      ;; Trigger on for one tick
      (funcall (getf module :update) :input 5.0)
      (assert-equality #'= 2.0 (funcall (getf module :get-output) :output))
      (funcall (getf module :update) :input 5.0)
      (assert-equality #'= 0.0 (funcall (getf module :get-output) :output))))


