(in-package :cl-synthesizer-test)

(defun make-envelope-environment ()
    (cl-synthesizer:make-environment :sample-rate 1000))

#|
test-case:
(
:segments (...)
:test-cases (
(:gate :on :ticks 100 :expected-cv 5.0)
|#

(defun run-test-case-envelope (test-case)
  (let ((module (cl-synthesizer-modules-envelope:envelope
		 "Envelope Module"
		 (make-envelope-environment)
		 :segments
		 (getf test-case :segments))))
    (dolist (cur-test-case (getf test-case :test-cases))
      (let ((gate (if (eq :on (getf cur-test-case :gate)) 5.0 0.0))
	    (ticks (getf cur-test-case :ticks))
	    (expected-cv (getf cur-test-case :expected-cv)))
	(dotimes (i ticks)
	  (funcall (getf module :update) :gate gate))
	(assert-equal expected-cv (funcall (getf module :get-output) :cv))))))

#|
(define-test test-envelope-1 ()
	     (let ((test
		    '(:segments ((:name "Attack" :time-ms 50 :target-cv 5 :requires-gate t))
		      :test-cases ((:gate :on :ticks 50 :expected-cv 5)))))
	       (run-test-case-envelope test)))
|#
