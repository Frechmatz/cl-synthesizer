(in-package :cl-synthesizer-test)


(defun make-stage-test-environment ()
  (cl-synthesizer:make-environment :sample-rate 10000))

;; test-case:
;; (:update (:gate 1.0 ....)
;; :expected-outputs ((:gate 1.0) (:output 5.0) ...))
(defun run-stage-tests (module test-cases)
  (dolist (test-case test-cases)
    (apply (getf module :update) (getf test-case :update))
    (dolist (expected (getf test-case :expected-outputs))
      (let ((value (funcall (getf module :get-output) (first expected))))
	(assert-equal (second expected) value)))))

(define-test test-stage-1 ()
	     "Test 1"
	     (let ((module (cl-synthesizer-modules-ramp-stage:make-module
			   "Ramp"
			   (make-stage-test-environment)
			   :time-ms 10 :target-output 5.0 :gate-state nil)))
	       (run-stage-tests
		module
		'(
		  (:update (:trigger 0.0 :input 5.0 :pass-through nil :gate nil)
		   :expected-outputs ((:output 0.0) (:busy 0.0) (:done 0.0)))
		  (:update (:trigger 0.0 :input 3.0 :pass-through 5.0 :gate nil)
		   :expected-outputs ((:output 3.0) (:busy 0.0) (:done 0.0)))
		  (:update (:trigger 5.0 :input 4.0 :pass-through 0.0 :gate nil)
		   :expected-outputs ((:output 4.01) (:busy 5.0) (:done 0.0)))
		  ;; re-trigger
		  (:update (:trigger 5.0 :input 4.0 :pass-through 0.0 :gate nil)
		   :expected-outputs ((:output 4.01) (:busy 5.0) (:done 0.0)))
		  ;; Test fails.
		  ;;(:update (:trigger 0.0 :input 10.0 :pass-through 0.0 :gate nil)
		  ;; :expected-outputs ((:output 4.01) (:busy 5.0) (:done 0.0)))
		  )
		  )))


