(in-package :cl-synthesizer-test)

(defun make-envelope-environment ()
  (cl-synthesizer:make-environment :sample-rate 1000))

(defun run-test-case-envelope (test-case)
  (let ((envelope-segments
	 (mapcar (lambda (s)
		   (let ((duration-controller (getf s :duration-controller))
			 ;;(target-cv-controller (getf s :target-cv-controller))
			 (mapped
			  (list
			   :duration-ms (getf s :duration-ticks)
			   :target-cv (getf s :target-cv)
			   :required-gate-state (getf s :required-gate-state))))
		     (if duration-controller
			 (progn
			   (push
			    (list
			     :socket (getf duration-controller :socket)
			     :input-min (getf duration-controller :input-min)
			     :input-max (getf duration-controller :input-max)
			     :output-min (getf duration-controller :output-min-ticks)
			     :output-max (getf duration-controller :output-max-ticks))
			    mapped)
			   (push :duration-controller mapped)))
		     mapped))
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
	    (apply (getf module :update) (concatenate 'list (list :gate gate) (getf cur-test-case :controller-inputs))))
	  ;; https://stackoverflow.com/questions/547436/whats-the-difference-between-eq-eql-equal-and-equalp-in-common-lisp
	  (assert-equality #'= expected-cv (float (funcall (getf module :get-output) :cv))))))))

(define-test test-envelope-1 ()
	     (let ((test
		    '(:segments ((:duration-ticks 50 :target-cv 5 :required-gate-state :on))
		      :test-cases ((:gate :on :ticks 50 :expected-cv 5)))))
	       (run-test-case-envelope test)))

(define-test test-envelope-2 ()
	     (let ((test
		    `(:segments ((:duration-ticks 10 :target-cv 5 :required-gate-state :on))
		      ;; CV must climb on first tick
		      :test-cases ((:gate :on :ticks 1 :expected-cv 0.5)))))
	       (run-test-case-envelope test)))

(define-test test-envelope-3 ()
	     (let ((test
		    '(:segments ((:duration-ticks 0 :target-cv 5 :required-gate-state :on)
				 (:duration-ticks 10 :target-cv 5 :required-gate-state :on))
		      :test-cases ((:gate :on :ticks 1 :expected-cv 0.5)))))
	       (run-test-case-envelope test)))

(define-test test-envelope-jumping-attack ()
	     (let ((test
		    '(:segments ((:duration-ticks nil :target-cv 5 :required-gate-state :on))
		      :test-cases ((:gate :on :ticks 1 :expected-cv 5)))))
	       (run-test-case-envelope test)))

(define-test test-envelope-jumping-segments ()
	     (let ((test
		    '(:segments ((:duration-ticks nil :target-cv 5 :required-gate-state :on)
				 (:duration-ticks nil :target-cv 20 :required-gate-state :off))
		      :test-cases ((:gate :on :ticks 1 :expected-cv 5)
				   (:gate :off :ticks 1 :expected-cv 20)))))
	       (run-test-case-envelope test)))

(define-test test-envelope-adsr ()
	     (let ((test
		    '(:segments ((:duration-ticks 1000 :target-cv 100 :required-gate-state :on)
				 (:duration-ticks 1000 :target-cv 50 :required-gate-state :on)
				 (:required-gate-state :on)
				 (:duration-ticks 1000 :target-cv 0 :required-gate-state :off))
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

(define-test test-envelope-controller-1 ()
	     (let ((test
		    '(:segments ((:duration-ticks 1000 :target-cv 100 :required-gate-state :on
				  :duration-controller
				  (:socket :attack-duration
					   :input-min -5.0
					   :input-max 5.0
					   :output-min-ticks -500
					   :output-max-ticks 500))
				 (:duration-ticks 1000 :target-cv 50 :required-gate-state :on)
				 (:required-gate-state :on)
				 (:duration-ticks 1000 :target-cv 0 :required-gate-state :off))
		      ;; decrease duration by 500 ticks
		      :test-cases ((:gate :on :ticks 1 :expected-cv 0.2 :controller-inputs (:attack-duration -5.0))
				   ))))
	       (run-test-case-envelope test)))

(define-test test-envelope-controller-2 ()
	     (let ((test
		    '(:segments ((:duration-ticks 1000 :target-cv 100 :required-gate-state :on
				  :duration-controller
				  (:socket :attack-duration
					   :input-min -5.0
					   :input-max 5.0
					   :output-min-ticks -1000
					   :output-max-ticks 1000))
				 (:duration-ticks 1000 :target-cv 50 :required-gate-state :on)
				 (:required-gate-state :on)
				 (:duration-ticks 1000 :target-cv 0 :required-gate-state :off))
		      ;; increase duration by 1000 ticks
		      :test-cases ((:gate :on :ticks 1 :expected-cv 0.05 :controller-inputs (:attack-duration 5.0))
				   ))))
	       (run-test-case-envelope test)))
