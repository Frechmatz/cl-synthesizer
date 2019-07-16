(in-package :cl-synthesizer-test)

(define-test vca-test-1-1 ()
	     (let ((vca (cl-synthesizer-modules-vca:make-module
			 "VCA"
			 (cl-synthesizer:make-environment)
			 :cv-max 5.0
			 :exponential nil)))
	       (update-module vca (list
			:input 2.5
			:cv 5.0
			:gain 0.0))

	       (assert-equality
		#'= 2.5
		(float (get-module-output vca :output)))))

(define-test vca-test-1-2 ()
	     (let ((vca (cl-synthesizer-modules-vca:make-module
			 "VCA"
			 (cl-synthesizer:make-environment)
			 :cv-max 5.0
			 :exponential t)))
	       (update-module vca (list
			:input 2.5
			:cv 5.0
			:gain 0.0))
	       (let ((output (get-module-output vca :output)))
		 (assert-true (is-approximately 2.5 output 0.01)))))

(define-test vca-test-2 ()
	     (let ((vca (cl-synthesizer-modules-vca:make-module
			 "VCA"
			 (cl-synthesizer:make-environment)
			 :cv-max 5.0
			 :exponential t)))
	     (update-module vca (list 
			:input 10.0
			:cv 5.0
			:gain 0.0))
	       (let ((output (get-module-output vca :output)))
		 (assert-true (is-approximately 10.0 output 0.1)))))

(define-test vca-test-3 ()
	     (let ((vca (cl-synthesizer-modules-vca:make-module
			 "VCA"
			 (cl-synthesizer:make-environment)
			 :cv-max 10.0
			 :initial-gain 9.0
			 :exponential nil)))
	     (update-module vca (list
			:input 1.0
			:cv -5.0
			:gain 0.0))
	       (assert-equality
		#'= 0.4
		(get-module-output vca :output))))
