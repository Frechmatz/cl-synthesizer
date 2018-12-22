(in-package :cl-synthesizer-test)


(defparameter *ramp-test-sample-rate* 10000)
(defparameter *ramp-test-ticks-per-ms* (/ *ramp-test-sample-rate* 1000))

(defun make-ramp-test-environment ()
  (cl-synthesizer:make-environment :sample-rate *ramp-test-sample-rate*))

;; test-case:
;; (:update (:gate 1.0 ....)
;; :expected-outputs ((:gate 1.0) (:output 5.0) ...))
(defun run-ramp-tests (module test-cases)
  (dolist (test-case test-cases)
    ;; fill in missing input arguments with nil
    (let ((update-args nil))
      (dolist (key (funcall (getf module :inputs)))
	(push (getf (getf test-case :update) key) update-args)
	(push key update-args))
      (funcall (getf module :update) update-args)
      (dolist (expected (getf test-case :expected-outputs))
	(let ((value (funcall (getf module :get-output) (first expected))))
	  (assert-equal (second expected) value))))))


(define-test test-ramp-gate-passthrough ()
	     ""
	     (let* ((module (cl-synthesizer-modules-ramp:make-module
			     "Ramp"
			     (make-ramp-test-environment)
			     :time-ms 10 :target-output 99.0 :gate-state nil)))
	       (run-ramp-tests
		module
		`((:update (:trigger 0.0 :input 10.0)
			   :expected-outputs ((:gate nil)))
		  (:update (:trigger 0.0 :input 10.0 :gate 0.0)
			   :expected-outputs ((:gate 0.0)))
		  (:update (:trigger 0.0 :input 10.0 :gate 5.0)
			   :expected-outputs ((:gate 5.0)))))))

(define-test test-ramp-initial-output ()
	     ""
	     (let* ((module (cl-synthesizer-modules-ramp:make-module
			     "Ramp"
			     (make-ramp-test-environment)
			     :time-ms 10 :target-output 99.0 :gate-state nil)))
	       (run-ramp-tests
		module
		`((:update (:trigger 0.0 :input 10.0)
		   :expected-outputs ((:output 0.0)))))))

(define-test test-ramp-passthrough-precedence ()
	     "Passthrough input has higher precedence than trigger input"
	     (let* ((module (cl-synthesizer-modules-ramp:make-module
			     "Ramp"
			     (make-ramp-test-environment)
			     :time-ms 10 :target-output 99.0 :gate-state nil)))
	       (run-ramp-tests
		module
		`((:update (:trigger 5.0 :input 10.0 :pass-through 5.0)
			   :expected-outputs ((:output 10.0)))))))

(define-test test-ramp-gate-on ()
	     ""
	     (let* ((module
		     (cl-synthesizer-modules-ramp:make-module
		      "Ramp"
		      (make-ramp-test-environment)
		      :time-ms 10 :target-output 99.0 :gate-state :on)))
	       (run-ramp-tests
		module
		`((:update (:trigger 5.0 :input 10.0)
			   :expected-outputs ((:output 10.0) (:busy 0.0) (:done 5.0)))))))

(define-test test-ramp-gate-off ()
	     ""
	     (let* ((module
		     (cl-synthesizer-modules-ramp:make-module
		      "Ramp"
		      (make-ramp-test-environment)
		      :time-ms 10 :target-output 99.0 :gate-state :off)))
	       (run-ramp-tests
		module
		`((:update (:trigger 5.0 :input 10.0 :gate 5.0)
			   :expected-outputs ((:output 10.0) (:busy 0.0) (:done 5.0)))))))

(define-test test-ramp-done-trigger ()
	     "Test that done output behaves as trigger"
	     (let* ((module
		     (cl-synthesizer-modules-ramp:make-module
		      "Ramp"
		      (make-ramp-test-environment)
		      :time-ms 10 :target-output 99.0 :gate-state :on)))
	       (run-ramp-tests
		module
		`((:update (:trigger 5.0 :input 10.0)
			   :expected-outputs ((:output 10.0) (:busy 0.0) (:done 5.0)))
		  ;; done must go to 0.0
		  (:update (:trigger 0.0 :input 10.0)
			   :expected-outputs ((:output 10.0) (:busy 0.0) (:done 0.0)))))))

(define-test test-ramp-climb ()
	     ""
	     (let* ((time-ms 10) (target-output 10.0)
		    (module (cl-synthesizer-modules-ramp:make-module
			   "Ramp"
			   (make-ramp-test-environment)
			   :time-ms time-ms :target-output target-output :gate-state nil))
		   (deltaTick (/ target-output (* time-ms *ramp-test-ticks-per-ms*))))
	       (run-ramp-tests
		module
		`((:update (:trigger 0.0 :input 10.0)
		   :expected-outputs ((:output 0.0) (:busy 0.0) (:done 0.0)))
		  (:update (:trigger 0.0 :input 3.0 :pass-through 5.0)
		   :expected-outputs ((:output 3.0) (:busy 5.0) (:done 0.0)))
		  (:update (:trigger 0.0 :input 0.0 :pass-through 5.0)
		   :expected-outputs ((:output 0.0) (:busy 5.0) (:done 0.0)))
		  (:update (:trigger 5.0 :input 0.0 :pass-through 0.0)
		   :expected-outputs ((:output ,(* 1 deltaTick)) (:busy 5.0) (:done 0.0)))
		  ;; re-trigger (ramp must start from beginning)
		  (:update (:trigger 5.0 :input 0.0 :pass-through 0.0)
			   :expected-outputs ((:output ,(* 1 deltaTick)) (:busy 5.0) (:done 0.0)))
		  ;; continue ramp
		  (:update (:trigger 0.0 :input 10.0 :pass-through 0.0)
		   :expected-outputs ((:output ,(* 2 deltaTick)) (:busy 5.0) (:done 0.0)))))))

(define-test test-ramp-climb-done ()
	     ""
	     (let* ((time-ms 10) (target-output 10.0)
		    (module (cl-synthesizer-modules-ramp:make-module
			   "Ramp"
			   (make-ramp-test-environment)
			   :time-ms time-ms :target-output target-output :gate-state nil))
		   (deltaTick (/ target-output (* time-ms *ramp-test-ticks-per-ms*))))
	       (run-ramp-tests
		module
		`((:update (:trigger 0.0 :input 10.0)
		   :expected-outputs ((:output 0.0) (:busy 0.0) (:done 0.0)))
		  (:update (:trigger 0.0 :input 3.0 :pass-through 5.0)
		   :expected-outputs ((:output 3.0) (:busy 5.0) (:done 0.0)))
		  (:update (:trigger 0.0 :input 0.0 :pass-through 5.0)
		   :expected-outputs ((:output 0.0) (:busy 5.0) (:done 0.0)))
		  (:update (:trigger 5.0 :input 0.0 :pass-through 0.0)
			   :expected-outputs ((:output ,(* 1 deltaTick)) (:busy 5.0) (:done 0.0)))))
	       
		  (dotimes (i 20000)
		    ;; continue ramp
		    (run-ramp-tests
		     module
		     `((:update (:trigger 0.0 :input 10.0 :pass-through 0.0)
			      :expected-outputs nil))))
		  (assert-true (is-approximately 10.0 (funcall (getf module :get-output) :output) 0.001))
		  (assert-equal 0.0 (funcall (getf module :get-output) :busy))))


(define-test test-ramp-climb-gate ()
	     ""
	     (let* ((time-ms 10) (target-output 10.0)
		    (module (cl-synthesizer-modules-ramp:make-module
			   "Ramp"
			   (make-ramp-test-environment)
			   :time-ms time-ms :target-output target-output :gate-state :on))
		   (deltaTick (/ target-output (* time-ms *ramp-test-ticks-per-ms*))))
	       (run-ramp-tests
		module
		`((:update (:trigger 5.0 :input 0.0 :pass-through 0.0 :gate 5.0)
		   :expected-outputs ((:output ,(* 1 deltaTick)) (:busy 5.0) (:done 0.0)))
		  ;; continue ramp
		  (:update (:trigger 0.0 :input 10.0 :pass-through 0.0 :gate 5.0)
			   :expected-outputs ((:output ,(* 2 deltaTick)) (:busy 5.0) (:done 0.0)))
		  ;; abort ramp via gate
		  (:update (:trigger 0.0 :input 10.0 :pass-through 0.0 :gate 0.0)
			   :expected-outputs ((:output ,(* 2 deltaTick)) (:busy 0.0) (:done 5.0)))))))


(define-test test-ramp-modulate-time-1 ()
	     "Change gradient of ramp by modulating the time"
	     (let* ((time-ms 10) (target-output 10.0)
		    (module (cl-synthesizer-modules-ramp:make-module
			   "Ramp"
			   (make-ramp-test-environment)
			   :time-ms time-ms :target-output target-output :gate-state :on
			   :time-cv-to-time-ms (lambda(time-cv) (* (abs time-cv) 1000))))
		   (deltaTick (/ target-output (* time-ms *ramp-test-ticks-per-ms*))))
	       (run-ramp-tests
		module
		`((:update (:trigger 5.0 :input 0.0 :pass-through 0.0 :gate 5.0)
		   :expected-outputs ((:output ,(* 1 deltaTick)) (:busy 5.0) (:done 0.0)))
		  (:update (:trigger 0.0 :input 10.0 :pass-through 0.0 :gate 5.0
				     ;; Change time from 10ms to 1000ms (100 times longer)
				     :cv-time 1.0 )
			   :expected-outputs ((:output ,(+ deltaTick (* 0.01 deltaTick))) (:busy 5.0) (:done 0.0)))
		  (:update (:trigger 0.0 :input 10.0 :pass-through 0.0 :gate 5.0
				     ;; Change time back to 10ms
				     :cv-time 0.01 )
			   :expected-outputs ((:output ,(+ (* 2 deltaTick) (* 0.01 deltaTick))) (:busy 5.0) (:done 0.0)))
		  ))))

(define-test test-ramp-modulate-time-2 ()
	     "Abort ramp by setting time to 0.0. Output must not jump to 10.0"
	     (let* ((time-ms 10) (target-output 10.0)
		    (module (cl-synthesizer-modules-ramp:make-module
			   "Ramp"
			   (make-ramp-test-environment)
			   :time-ms time-ms :target-output target-output :gate-state :on
			   :time-cv-to-time-ms (lambda(time-cv) (* (abs time-cv) 1000))))
		   (deltaTick (/ target-output (* time-ms *ramp-test-ticks-per-ms*))))
	       (run-ramp-tests
		module
		`((:update (:trigger 5.0 :input 0.0 :pass-through 0.0 :gate 5.0)
		   :expected-outputs ((:output ,(* 1 deltaTick)) (:busy 5.0) (:done 0.0)))
		  (:update (:trigger 0.0 :input 10.0 :pass-through 0.0 :gate 5.0
				     ;; Change time to 0.0 (abort ramp)
				     :cv-time 0.0 )
			   :expected-outputs ((:output ,(* 1 deltaTick)) (:busy 0.0) (:done 5.0)))
		  ))))


