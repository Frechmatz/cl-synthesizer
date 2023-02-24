(in-package :cl-synthesizer-test)

;; test-case:
;; (:update (:gate 1.0 ....)
;; :expected-outputs ((:gate 1.0) (:output 5.0) ...))
(defun run-sustain-tests (module test-cases)
  (dolist (test-case test-cases)
    ;; fill in missing input arguments with nil
    (let ((update-args nil))
      (dolist (key (get-module-input-sockets module))
	(push
	 (list
	  key
	  (getf (getf test-case :update) key))
	 update-args))
      (update-module module update-args)
      (dolist (expected (getf test-case :expected-outputs))
	(let ((value (get-module-output module (first expected))))
	  (assert-equal (second expected) value))))))


(define-test test-sustain-gate-passthrough ()
	     (let* ((module (cl-synthesizer-modules-sustain:make-module
			     "Sustain"
			     (cl-synthesizer:make-environment))))
	       (run-sustain-tests
		module
		`((:update (:trigger 0.0 :input 10.0)
			   :expected-outputs ((:gate nil)))
		  (:update (:trigger 0.0 :input 10.0 :gate 0.0)
			   :expected-outputs ((:gate 0.0)))
		  (:update (:trigger 0.0 :input 10.0 :gate 5.0)
			   :expected-outputs ((:gate 5.0)))))))

(define-test test-sustain-initial-output ()
	     (let* ((module (cl-synthesizer-modules-sustain:make-module
			     "Sustain"
			     (cl-synthesizer:make-environment))))
	       (run-sustain-tests
		module
		`((:update (:trigger 0.0 :input 10.0)
		   :expected-outputs ((:output 0.0)))))))

(define-test test-sustain-passthrough-precedence ()
	     "Passthrough input has higher precedence than trigger input"
	     (let* ((module (cl-synthesizer-modules-sustain:make-module
			     "Sustain"
			     (cl-synthesizer:make-environment))))
	       (run-sustain-tests
		module
		`((:update (:trigger 5.0 :input 10.0 :pass-through 5.0)
			   :expected-outputs ((:output 10.0)))))))

(define-test test-sustain-gate-on ()
	     (let* ((module
		     (cl-synthesizer-modules-sustain:make-module
		      "Sustain"
		      (cl-synthesizer:make-environment))))
	       (run-sustain-tests
		module
		`((:update (:trigger 5.0 :input 10.0)
			   :expected-outputs ((:output 10.0) (:busy 0.0) (:done 5.0)))))))

(define-test test-sustain-gate-off ()
	     (let* ((module
		     (cl-synthesizer-modules-sustain:make-module
		      "Sustain"
		      (cl-synthesizer:make-environment))))
	       (run-sustain-tests
		module
		`((:update (:trigger 5.0 :input 10.0 :gate 0.0)
			   :expected-outputs ((:output 10.0) (:busy 0.0) (:done 5.0)))))))

(define-test test-sustain-done-trigger ()
	     "Test that done output behaves as trigger"
	     (let* ((module
		     (cl-synthesizer-modules-sustain:make-module
		      "Sustain"
		      (cl-synthesizer:make-environment))))
	       (run-sustain-tests
		module
		`((:update (:trigger 5.0 :input 10.0)
			   :expected-outputs ((:output 10.0) (:busy 0.0) (:done 5.0)))
		  ;; done must go to 0.0
		  (:update (:trigger 0.0 :input 10.0)
			   :expected-outputs ((:output 10.0) (:busy 0.0) (:done 0.0)))))))

(define-test test-sustain-hold-done ()
	     (let* ((module (cl-synthesizer-modules-sustain:make-module
			     "Sustain"
			     (cl-synthesizer:make-environment))))
	       (run-sustain-tests
		module
		`((:update (:trigger 0.0 :input 10.0)
			   :expected-outputs ((:output 0.0) (:busy 0.0) (:done 0.0)))
		  (:update (:trigger 5.0 :input 10.0 :gate 5.0)
			   :expected-outputs ((:output 10.0) (:busy 5.0) (:done 0.0)))
		  (:update (:trigger 0.0 :input 50.0 :gate 5.0)
			   :expected-outputs ((:output 10.0) (:busy 5.0) (:done 0.0)))
		  (:update (:trigger 0.0 :input 70.0 :gate 5.0)
			   :expected-outputs ((:output 10.0) (:busy 5.0) (:done 0.0)))
		  (:update (:trigger 0.0 :input 70.0 :gate 0.0)
			   :expected-outputs ((:output 10.0) (:busy 0.0) (:done 5.0)))))))
