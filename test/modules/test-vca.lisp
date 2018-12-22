(in-package :cl-synthesizer-test)

(define-test test-vca-core-1 ()
    (let* (
	   (core (cl-synthesizer-modules-vca::vca-core))
	   (core-lin (getf core :linear))
	   (core-exp (getf core :exponential)))
      (assert-equal 1.0 (funcall core-lin 10.0))
      (let ((exp (funcall core-exp 10.0)))
	(assert (>= exp 0.99)))))

(define-test test-vca-core-2 ()
    (let* (
	   (core (cl-synthesizer-modules-vca::vca-core))
	   (core-lin (getf core :linear))
	   (core-exp (getf core :exponential)))
      (assert-equal 0.0 (funcall core-lin 0.0))
      (assert-equal 0.0 (funcall core-exp 0.0))))

(define-test test-vca-core-3 ()
    (let* (
	   (core (cl-synthesizer-modules-vca::vca-core))
	   (core-lin (getf core :linear))
	   (core-exp (getf core :exponential)))
      (expect-invalid-arguments-exception
	(funcall core-lin -1.0))))

(define-test test-vca-core-4 ()
    (let* (
	   (core (cl-synthesizer-modules-vca::vca-core))
	   (core-lin (getf core :linear))
	   (core-exp (getf core :exponential)))
      (expect-invalid-arguments-exception
	(funcall core-lin 11.0))))

(define-test vca-test-1 ()
	     (let ((vca (cl-synthesizer-modules-vca:make-module
			 "VCA"
			 (cl-synthesizer:make-environment)
			 :cv-max 5.0)))
	       (funcall (getf vca :update)
			:input 2.5
			:cv 5.0
			:gain 0.0)

	       (assert-equality
		#'= 2.5
		(float (funcall (getf vca :get-output) :output-linear)))

	       (let ((output-exp (float (funcall (getf vca :get-output) :output-exponential))))
		 (assert-true (and
			       (<= 2.49 output-exp)
			       (<= output-exp 2.51))))))

(define-test vca-test-2 ()
	     (let ((vca (cl-synthesizer-modules-vca:make-module
			 "VCA"
			 (cl-synthesizer:make-environment)
			 :cv-max 5.0)))
	     (funcall (getf vca :update)
			:input 10.0
			:cv 5.0
			:gain 0.0)
	       (let ((output-exp (float (funcall (getf vca :get-output) :output-exponential))))
		 (assert-true (and
			       (<= 9.9 output-exp)
			       (<= output-exp 10.1))))))

(define-test vca-test-3 ()
	     (let ((vca (cl-synthesizer-modules-vca:make-module
			 "VCA"
			 (cl-synthesizer:make-environment)
			 :cv-max 10.0
			 :initial-gain 9.0)))
	     (funcall (getf vca :update)
			:input 1.0
			:cv -5.0
			:gain 0.0)
	       (assert-equality
		#'= 0.4
		(float (funcall (getf vca :get-output) :output-linear)))))
