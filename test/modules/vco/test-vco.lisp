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


;; Add 6000Hz via linear CV input
(define-test vco-test-lin-1 ()
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
				       :cv-linear 2.5))
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 ;; Allow some deviation due to algorithm used by get-frequency
		 (format t "~%F: ~a~%" f)
		 ;; 6000 (linear) + 440 (base)
		 (assert-true (and
			       (<= 6439.5 f)
			       (<= f 6440.5))))))

;; Test clipping 
(define-test vco-test-frequency-clipping-upper ()
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
				       :cv-linear 5.0)) ;; this will cause frequency clipping
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 ;; Allow some deviation due to algorithm used by get-frequency
		 (format t "~%F: ~a~%" f)
		 (assert-true (and
			       (<= 11999.5 f)
			       (<= f 12000.5))))))

;; Test clipping of negative frequencies
(define-test vco-test-frequency-clipping-bottom ()
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
				       :cv-linear -5.0)) ;; this will cause frequency clipping
			 :get-output-fn (lambda ()
					  (funcall
					   (getf vco :get-output)
					   :sine)))))
		 (format t "~%F: ~a~%" f)
		 (assert-equal 0.0 f))))

