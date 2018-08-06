(in-package :cl-synthesizer-test)

;; Test that VCO emits base-frequency when cv input is 0.0
(define-test vco-test-1 ()
	     (let ((vco (cl-synthesizer-modules-vco:vco
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440
			 :cv-max 5
			 :f-max 12000
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv 0.0
				       :cv-linear 0.0))
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 ;; Allow some deviation due to algorithm used by get-frequency
		 (assert-true (and
			       (<= 439.5 f)
			       (<= f 440.5))))))

;; Test that output frequency of VCA goes up one octave when cv input is 1.0
(define-test vco-test-exp-1 ()
	     (let ((vco (cl-synthesizer-modules-vco:vco
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440
			 :cv-max 5
			 :f-max 12000
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv 1.0 ;; one octave up
				       :cv-linear 0.0))
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 ;; Allow some deviation due to algorithm used by get-frequency
		 (assert-true (and
			       (<= 879.5 f)
			       (<= f 880.5))))))

;; Test that output frequency of VCA goes up two octaves when cv input is 2.0
(define-test vco-test-exp-2 ()
	     (let ((vco (cl-synthesizer-modules-vco:vco
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440
			 :cv-max 5
			 :f-max 12000
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv 2.0 ;; two octaves up
				       :cv-linear 0.0))
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 ;; Allow some deviation due to algorithm used by get-frequency
		 (assert-true (and
			       (<= 1759.5 f)
			       (<= f 1760.5))))))


;; Test that output frequency of VCA goes down one octave when cv input is -1.0
(define-test vco-test-exp-3 ()
	     (let ((vco (cl-synthesizer-modules-vco:vco
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440
			 :cv-max 5
			 :f-max 12000
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv -1.0 ;; one octave down
				       :cv-linear 0.0))
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 ;; Allow some deviation due to algorithm used by get-frequency
		 (assert-true (and
			       (<= 219.5 f)
			       (<= f 220.5))))))


;; Test that output frequency of VCA goes down two octave when cv input is -2.0
(define-test vco-test-exp-4 ()
	     (let ((vco (cl-synthesizer-modules-vco:vco
			 "VCO"
			 (cl-synthesizer:make-environment)
			 :base-frequency 440
			 :cv-max 5
			 :f-max 12000
			 :v-peak 5)))
	       (let ((f (cl-synthesizer-test::get-frequency
			 :sample-rate 44100
			 :update-fn (lambda()
				      (funcall
				       (getf vco :update)
				       :cv -2.0 ;; two octaves down
				       :cv-linear 0.0))
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 ;; Allow some deviation due to algorithm used by get-frequency
		 (assert-true (and
			       (<= 109.5 f)
			       (<= f 110.5))))))

