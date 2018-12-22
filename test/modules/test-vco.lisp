(in-package :cl-synthesizer-test)

(defun print-vco-state (vco)
  (format t "~%VCO: f-exp: ~a f-lin: ~a f: ~a~%"
	  (funcall (getf vco :get-state) :exponential-frequency)
	  (funcall (getf vco :get-state) :linear-frequency)
	  (funcall (getf vco :get-state) :frequency)))

;; Test that VCO emits base-frequency when cv inputs are 0.0
(define-test vco-test-1-0 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440
			 :f-max 12000
			 :cv-max 5.0
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv-lin 0.0 :cv-exp 0.0))
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 (print-vco-state vco)
		 (assert-true (is-approximately 440.0 f 0.5)))))

;; Test that VCO emits base-frequency when cv inputs are 0.0 and base-frequency is initialized with negative frequency
(define-test vco-test-1-1 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency -440
			 :f-max 12000
			 :cv-max 5.0
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv-lin 0.0 :cv-exp 0.0))
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 (print-vco-state vco)
		 (assert-true (is-approximately 440.0 f 0.5)))))


;; Test that frequency of VCO goes up one octave when cv-exp input is 1.0
(define-test vco-test-exp-1 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440
			 :cv-max 5.0
			 :f-max 12000
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv-exp 1.0 :cv-lin 0.0  ;; one octave up
				       ))
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 (assert-true (is-approximately 880.0 f 0.5)))))

;; Test that frequency of VCO goes up two octaves when cv-exp input is 2.0
(define-test vco-test-exp-2 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440
			 :f-max 12000
			 :cv-max 5.0
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv-exp 2.0)) ;; two octaves up
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 (assert-true (is-approximately 1760.0 f 0.5)))))

;; Test that frequency of VCO goes down one octave when cv input is -1.0
(define-test vco-test-exp-3 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440
			 :f-max 12000
			 :cv-max 5.0
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv-exp -1.0 :cv-lin 0.0)) ;; one octave down
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 (assert-true (is-approximately 220.0 f 0.5)))))

;; Test that frequency of VCO goes down two octave when cv-exp input is -2.0
(define-test vco-test-exp-4 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440
			 :f-max 12000
			 :cv-max 5.0
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv-exp -2.0 :cv-lin 0.0)) ;; two octaves down
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 (assert-true (is-approximately 110.0 f 0.5)))))

;; Add 6000Hz via linear CV input
(define-test vco-test-lin-1 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :cv-max 5
			 :f-max 12000
			 :cv-max 5.0
			 :base-frequency 0
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv-lin 2.5 :cv-exp 0.0))
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 (print-vco-state vco)
		 (assert-true (is-approximately 6000 f 0.5)))))

;; Add 6000Hz to 440Hz via linear CV input
(define-test vco-test-lin-2 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :cv-max 5
			 :f-max 12000
			 :base-frequency 440
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv-lin 2.5 :cv-exp 0.0))
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 (print-vco-state vco)
		 (assert-true (is-approximately 6440 f 0.5)))))

;; Test frequency clipping (exp input)
(define-test vco-test-frequency-clipping-exp-upper ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440
			 :f-max 12000
			 :cv-max 5.0
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv-exp 20.0 :cv-lin 0.0))
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 (print-vco-state vco)
		 (assert-true (is-approximately 12000 f 0.5)))))

;; Test frequency clipping (lin input)
(define-test vco-test-frequency-clipping-lin-upper ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440
			 :f-max 12000
			 :cv-max 5.0
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv-exp 0.0 :cv-lin 5.0))
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 (print-vco-state vco)
		 (assert-true (is-approximately 12000 f 0.5)))))


;; Test lower clipping exp input
(define-test vco-test-frequency-clipping-exp-bottom ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency -5000
			 :f-max 12000
			 :cv-max 5.0
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv-exp 10.0 :cv-lin 0.0)) 
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 (print-vco-state vco)
		 ;; Frequency calculated by frequency counter
		 (assert-true (is-approximately 12000 f 0.5))
		 ;; Internal frequency of module
		 (assert-true (is-approximately -12000 (funcall (getf vco :get-state) :frequency) 0.5)))))

;; Test lower clipping lin input
(define-test vco-test-frequency-clipping-lin-bottom ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440
			 :f-max 12000
			 :cv-max 5.0
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv-lin -20.0 :cv-exp 0.0)) 
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 (print-vco-state vco)
		 ;; Frequency calculated by frequency counter
		 (assert-true (is-approximately 12000 f 0.5))
		 ;; Internal frequency of module
		 (assert-true (is-approximately -12000 (funcall (getf vco :get-state) :frequency) 0.5)))))

;; Test updating both cv inputs
(define-test vco-test-fm-1 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440
			 :f-max 12000
			 :cv-max 5.0
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv-exp 1.0 :cv-lin 1.0))
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 (print-vco-state vco)
		 ;; (+ 880 (/ 12000 5)) = 880 + 2400 = 3280
		 (assert-true (is-approximately 3280 f 0.5)))))

;; Test updating both cv inputs with zero crossing
(define-test vco-test-fm-2 ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440
			 :f-max 12000
			 :cv-max 5.0
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv-exp 1.0 :cv-lin -4.0))
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 (print-vco-state vco)
		 ;; 880 - 9600 = -8720
		 (assert-true (is-approximately 8720 f 0.5))
		 (assert-true (is-approximately -8720 (funcall (getf vco :get-state) :frequency) 0.5)))))

;; This test assumes, that the square wave output is 5.0 for 0 <= Phi < PI
;; and -5.0 for PI < Phi < 2PI 
(define-test vco-test-through-zero ()
	     (let ((vco (cl-synthesizer-modules-vco:make-module
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 0
			 :f-max 12000
			 :cv-max 5.0
			 :v-peak 5
			 :duty-cycle 0.5
			 )))
	       (funcall (getf vco :update) :cv-lin 0.5 :cv-exp 0)
	       (print-vco-state vco)
	       (assert-equality #'= 5.0 (funcall (getf vco :get-output) :square))
	       ;; Back to zero
	       (funcall (getf vco :update) :cv-lin -0.5 :cv-exp 0)
	       ;; One more step back
	       (funcall (getf vco :update) :cv-lin -0.5 :cv-exp 0)
	       (print-vco-state vco)
	       (assert-equality #'= -5.0 (funcall (getf vco :get-output) :square))))

