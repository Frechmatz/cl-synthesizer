(in-package :cl-synthesizer-test)

(defun make-envelope-environment ()
  (cl-synthesizer:make-environment :sample-rate 1000))

#|
test-case:
(
 :segments (...)
 :test-cases (
   (:gate :on :ticks 100 :expected-cv 5.0)
)
|#

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
	  ;; http://www.lispworks.com/documentation/HyperSpec/Body/f_equalp.htm
	  (assert-equality #'= expected-cv (funcall (getf module :get-output) :cv)))))))

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
