(in-package :cl-synthesizer-test)

(defun make-envelope-environment ()
  (cl-synthesizer:make-environment :sample-rate 1000))

(defun run-test-case-envelope (test-case)
  (let ((envelope-segments
	 (mapcar (lambda (s)
		   (list
		    ;; convert ticks to milliseconds
		    :time-ms (getf s :time-ticks)
		    :target-cv (getf s :target-cv)
		    :required-gate-state (getf s :required-gate-state)))
		 (getf test-case :segments))))
    (let ((module (cl-synthesizer-modules-envelope:envelope
		   "Envelope Module"
		   (make-envelope-environment)
		   :segments
		   envelope-segments)))
      (dolist (cur-test-case (getf test-case :test-cases))
	(let ((gate (if (eq :on (getf cur-test-case :gate)) 5.0 0.0))
	      (ticks (getf cur-test-case :ticks))
	      (expected-cv (getf cur-test-case :expected-cv)))
	  (dotimes (i ticks)
	    (funcall (getf module :update) :gate gate))
	  ;; https://stackoverflow.com/questions/547436/whats-the-difference-between-eq-eql-equal-and-equalp-in-common-lisp
	  (assert-equality #'= expected-cv (float (funcall (getf module :get-output) :cv))))))))

(define-test test-envelope-1 ()
	     (let ((test
		    '(:segments ((:time-ticks 50 :target-cv 5 :required-gate-state :on))
		      :test-cases ((:gate :on :ticks 50 :expected-cv 5)))))
	       (run-test-case-envelope test)))

(define-test test-envelope-2 ()
	     (let ((test
		    `(:segments ((:time-ticks 10 :target-cv 5 :required-gate-state :on))
		      ;; CV must climb on first tick
		      :test-cases ((:gate :on :ticks 1 :expected-cv 0.5)))))
	       (run-test-case-envelope test)))

(define-test test-envelope-3 ()
	     (let ((test
		    '(:segments ((:time-ticks 0 :target-cv 5 :required-gate-state :on)
				 (:time-ticks 10 :target-cv 5 :required-gate-state :on))
		      :test-cases ((:gate :on :ticks 1 :expected-cv 0.5)))))
	       (run-test-case-envelope test)))

(define-test test-envelope-jumping-attack ()
	     (let ((test
		    '(:segments ((:time-ticks nil :target-cv 5 :required-gate-state :on))
		      :test-cases ((:gate :on :ticks 1 :expected-cv 5)))))
	       (run-test-case-envelope test)))

(define-test test-envelope-jumping-segments ()
	     (let ((test
		    '(:segments ((:time-ticks nil :target-cv 5 :required-gate-state :on)
				 (:time-ticks nil :target-cv 20 :required-gate-state :off))
		      :test-cases ((:gate :on :ticks 1 :expected-cv 5)
				   (:gate :off :ticks 1 :expected-cv 20)))))
	       (run-test-case-envelope test)))

(define-test test-envelope-adsr ()
	     (let ((test
		    '(:segments ((:time-ticks 1000 :target-cv 100 :required-gate-state :on)
				 (:time-ticks 1000 :target-cv 50 :required-gate-state :on)
				 (:required-gate-state :on)
				 (:time-ticks 1000 :target-cv 0 :required-gate-state :off))
		      :test-cases ((:gate :off :ticks 20 :expected-cv 0)
				   ;; attack
				   (:gate :on :ticks 1 :expected-cv 0.1)
				   (:gate :on :ticks 999 :expected-cv 100)
				   ;; decay
				   (:gate :on :ticks 1 :expected-cv 99.95)
				   (:gate :on :ticks 999 :expected-cv 50)
				   ;; sustain
				   (:gate :on :ticks 1 :expected-cv 50)
				   (:gate :on :ticks 1000 :expected-cv 50)
				   ;; release
				   (:gate :off :ticks 1 :expected-cv 49.95)
				   (:gate :off :ticks 999 :expected-cv 0)
				   ))))
	       (run-test-case-envelope test)))
