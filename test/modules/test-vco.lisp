(in-package :cl-synthesizer-test)

(defun print-vco-state (vco)
  (format t "~%VCO: f-exp: ~a f-lin: ~a f: ~a~%"
	  (cl-synthesizer-test::get-module-state vco :exponential-frequency)
	  (cl-synthesizer-test::get-module-astate vco :linear-frequency)
	  (cl-synthesizer-test::get-module-state vco :frequency)))

;; Test that VCO emits base-frequency when cv inputs are 0.0
(define-test vco-test-1-0 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440.0
			 :v-peak 5.0)))
	       (let ((f (cl-synthesizer-test::frequency-counter
			 :sample-rate 44100
			 :update-fn (lambda()
				      (update-module vco (list
							  (list :cv-lin 0.0)
							  (list :cv-exp 0.0))))
			 :get-output-fn (lambda ()
					  (get-module-output vco :sine)))))
		 ;;(print-vco-state vco)
		 (assert-true (is-approximately 440.0 f 0.5)))))

;; Test that VCO emits base-frequency when cv inputs are 0.0 and base-frequency is initialized with negative frequency
(define-test vco-test-1-1 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency -440.0
			 :v-peak 5.0)))
	       (let ((f (cl-synthesizer-test::frequency-counter
			 :sample-rate 44100
			 :update-fn (lambda() (update-module vco
							     (list
							      (list :cv-lin 0.0)
							      (list :cv-exp 0.0))))
			 :get-output-fn (lambda () (get-module-output vco :sine)))))
		 ;;(print-vco-state vco)
		 (assert-true (is-approximately 440.0 f 0.5)))))


;; Test that frequency of VCO goes up one octave when cv-exp input is 1.0
(define-test vco-test-exp-1 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440.0
			 :v-peak 5.0)))
	       (let ((f (cl-synthesizer-test::frequency-counter
			 :sample-rate 44100
			 :update-fn (lambda() (update-module vco
							     (list
							      (list :cv-exp 1.0)
							      (list :cv-lin 0.0))  ;; one octave up
							     ))
			 :get-output-fn (lambda () (get-module-output vco :sine)))))
		 (assert-true (is-approximately 880.0 f 0.5)))))

;; Test that frequency of VCO goes up two octaves when cv-exp input is 2.0
(define-test vco-test-exp-2 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440.0
			 :v-peak 5.0)))
	       (let ((f (cl-synthesizer-test::frequency-counter
			 :sample-rate 44100
			 :update-fn (lambda()
				      (update-module vco
						     (list
						      (list :cv-exp 2.0)))) ;; two octaves up
			 :get-output-fn (lambda () (get-module-output vco :sine)))))
		 (assert-true (is-approximately 1760.0 f 0.5)))))

;; Test that frequency of VCO goes down one octave when cv input is -1.0
(define-test vco-test-exp-3 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440.0
			 :v-peak 5.0)))
	       (let ((f (cl-synthesizer-test::frequency-counter
			 :sample-rate 44100
			 :update-fn (lambda()
				      (update-module vco
						     (list
						      (list :cv-exp -1.0)
						      (list :cv-lin 0.0)))) ;; one octave down
			 :get-output-fn (lambda () (get-module-output vco :sine)))))
		 (assert-true (is-approximately 220.0 f 0.5)))))

;; Test that frequency of VCO goes down two octave when cv-exp input is -2.0
(define-test vco-test-exp-4 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440.0
			 :v-peak 5.0)))
	       (let ((f (cl-synthesizer-test::frequency-counter
			 :sample-rate 44100
			 :update-fn (lambda()
				      (update-module vco
						     (list
						      (list :cv-exp -2.0)
						      (list :cv-lin 0.0)))) ;; two octaves down
			 :get-output-fn (lambda () (get-module-output vco :sine)))))
		 (assert-true (is-approximately 110.0 f 0.5)))))

;; Add 6000Hz via linear CV input
(define-test vco-test-lin-1 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :cv-lin-hz-v 2400.0
			 :base-frequency 0.0
			 :v-peak 5.0)))
	       (let ((f (cl-synthesizer-test::frequency-counter
			 :sample-rate 44100
			 :update-fn (lambda() (update-module vco
							     (list
							      (list :cv-lin 2.5)
							      (list :cv-exp 0.0))))
			 :get-output-fn (lambda () (get-module-output vco :sine)))))
		 ;;(print-vco-state vco)
		 (assert-true (is-approximately 6000 f 0.5)))))

;; Add 6000Hz to 440Hz via linear CV input
(define-test vco-test-lin-2 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :cv-lin-hz-v 2400.0
			 :base-frequency 440.0
			 :v-peak 5.0)))
	       (let ((f (cl-synthesizer-test::frequency-counter
			 :sample-rate 44100
			 :update-fn (lambda() (update-module vco
							     (list
							      (list :cv-lin 2.5)
							      (list :cv-exp 0.0))))
			 :get-output-fn (lambda () (get-module-output vco :sine)))))
		 ;;(print-vco-state vco)
		 (assert-true (is-approximately 6440 f 0.5)))))

;; Test frequency clipping (exp input)
(define-test vco-test-frequency-clipping-exp-upper ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440.0
			 :f-max 12000.0
			 :v-peak 5.0)))
	       (let ((f (cl-synthesizer-test::frequency-counter
			 :sample-rate 44100
			 :update-fn (lambda() (update-module vco
							     (list
							      (list :cv-exp 20.0)
							      (list :cv-lin 0.0))))
			 :get-output-fn (lambda () (get-module-output vco :sine)))))
		 ;;(print-vco-state vco)
		 (assert-true (is-approximately 12000 f 0.5)))))

;; Test frequency clipping (lin input)
(define-test vco-test-frequency-clipping-lin-upper ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440.0
			 :cv-lin-hz-v 3000.0
			 :f-max 12000.0
			 :v-peak 5.0)))
	       (let ((f (cl-synthesizer-test::frequency-counter
			 :sample-rate 44100
			 :update-fn (lambda() (update-module vco
							     (list
							      (list :cv-exp 0.0)
							      (list :cv-lin 5.0))))
			 :get-output-fn (lambda () (get-module-output vco :sine)))))
		 ;;(print-vco-state vco)
		 (assert-true (is-approximately 12000 f 0.5)))))


;; Test lower clipping exp input
(define-test vco-test-frequency-clipping-exp-bottom ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency -5000.0
			 :f-max 12000.0
			 :v-peak 5.0)))
	       (let ((f (cl-synthesizer-test::frequency-counter
			 :sample-rate 44100
			 :update-fn (lambda() (update-module vco
							     (list
							      (list :cv-exp 10.0)
							      (list :cv-lin 0.0))))
			 :get-output-fn (lambda () (get-module-output vco :sine)))))
		 ;;(print-vco-state vco)
		 ;; Frequency calculated by frequency counter
		 (assert-true (is-approximately 12000 f 0.5))
		 ;; Internal frequency of module
		 (assert-true (is-approximately
			       -12000
			       (cl-synthesizer-test::get-module-state vco :frequency) 0.5)))))

;; Test lower clipping lin input
(define-test vco-test-frequency-clipping-lin-bottom ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440.0
			 :f-max 12000.0
			 :cv-lin-hz-v 1000.0
			 :v-peak 5.0)))
	       (let ((f (cl-synthesizer-test::frequency-counter
			 :sample-rate 44100
			 :update-fn (lambda() (update-module vco
							     (list
							      (list :cv-lin -20.0)
							      (list :cv-exp 0.0))))
			 :get-output-fn (lambda () (get-module-output vco :sine)))))
		 ;;(print-vco-state vco)
		 ;; Frequency calculated by frequency counter
		 (assert-true (is-approximately 12000 f 0.5))
		 ;; Internal frequency of module
		 (assert-true (is-approximately
			       -12000
			       (cl-synthesizer-test::get-module-state vco :frequency) 0.5)))))

;; Test updating both cv inputs
(define-test vco-test-fm-1 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :cv-lin-hz-v 1000.0
			 :base-frequency 440.0
			 :v-peak 5.0)))
	       (let ((f (cl-synthesizer-test::frequency-counter
			 :sample-rate 44100
			 :update-fn (lambda() (update-module vco
							     (list
							      (list :cv-exp 1.0)
							      (list :cv-lin 1.0))))
			 :get-output-fn (lambda () (get-module-output vco :sine)))))
		 ;;(print-vco-state vco)
		 ;; (+ 880 1000) = 1880
		 (assert-true (is-approximately 1880 f 0.5)))))

;; Test updating both cv inputs with zero crossing
(define-test vco-test-fm-2 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :cv-lin-hz-v 1000.0
			 :base-frequency 440.0
			 :v-peak 5.0)))
	       (let ((f (cl-synthesizer-test::frequency-counter
			 :sample-rate 44100
			 :update-fn (lambda() (update-module vco
							     (list
							      (list :cv-exp 1.0)
							      (list :cv-lin -4.0))))
			 :get-output-fn (lambda () (get-module-output vco :sine)))))
		 ;;(print-vco-state vco)
		 ;; 880 - (* 4.0 1000.0) = -3120
		 (assert-true (is-approximately 3120 f 0.5))
		 (assert-true (is-approximately
			       -3120
			       (cl-synthesizer-test::get-module-state vco :frequency) 0.5)))))

;; This test assumes, that the square wave output is 5.0 for 0 <= Phi < PI
;; and -5.0 for PI < Phi < 2PI 
(define-test vco-test-through-zero ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :cv-lin-hz-v 1000.0
			 :base-frequency 0.0
			 :v-peak 5.0
			 :duty-cycle 0.5)))
	       (update-module vco (list (list :cv-lin 0.5) (list :cv-exp 0.0)))
	       ;;(print-vco-state vco)
	       (assert-equality #'= 5.0 (get-module-output vco :square))
	       ;; Back to zero
	       (update-module vco (list (list :cv-lin -0.5) (list :cv-exp 0.0)))
	       ;; One more step back
	       (update-module vco (list (list :cv-lin -0.5) (list :cv-exp 0.0)))
	       ;;(print-vco-state vco)
	       (assert-equality #'= -5.0 (get-module-output vco :square))))

(define-test vco-test-sync ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :cv-lin-hz-v 0.0
			 :base-frequency 440
			 :v-peak 5.0
			 :sync-threshold 2.5)))
	       (update-module vco nil)
	       (let ((initial-sine (get-module-output vco :sine)))
		 (dotimes (i 100)
		   (update-module vco nil))
		 (update-module vco (list (list :sync 5.0)))
		 (let ((cur-sine (get-module-output vco :sine)))
		   (assert-equality #'= initial-sine cur-sine)))))
