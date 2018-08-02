(in-package :cl-synthesizer-test)


#|
;; Max amplification linear
(define-test vca-test-1 ()
	     (let ((vca (cl-synthesizer-modules-vca::vca-ng
			 "VCA"
			 (cl-synthesizer:make-environment)
			 :input-max 10.0
			 :output-max 100.0
			 :cv-max 5.0
			 :cv-initial-gain 0.0)))
	     (funcall (getf vca :update)
			:input 10.0
			:cv 5.0
			:cv-gain 0.0)
	       (assert-equality
		#'= 100.0
		(float (funcall (getf vca :get-output) :output-linear)))
	       ))




(define-test vca-test-2 ()
	     (let ((vca (cl-synthesizer-modules-vca::vca-ng
			 "VCA"
			 (cl-synthesizer:make-environment)
			 :input-max 10.0
			 :output-max 100.0
			 :cv-max 5.0
			 :cv-initial-gain 0.0)))
	     (funcall (getf vca :update)
			:input 10.0
			:cv 5.0
			:cv-gain 0.0)
	       (assert-equality
		#'= 100.0
		(float (funcall (getf vca :get-output) :output-exponential)))))

(define-test vca-test-3 ()
	     (let ((vca (cl-synthesizer-modules-vca::vca-ng
			 "VCA"
			 (cl-synthesizer:make-environment)
			 :input-max 1.0
			 :output-max 5.0
			 :cv-max 10.0
			 :cv-initial-gain 9.0)))
	     (funcall (getf vca :update)
			:input 1.0
			:cv -5.0
			:cv-gain 0.0)
	       (assert-equality
		#'= 100.0
		(float (funcall (getf vca :get-output) :output-exponential)))
	       (assert-equality
		#'= 0.5
		(float (funcall (getf vca :get-output) :output-linear)))

	       ))
|#

(define-test test-vca-core-1 ()
    (let* (
	   (core (cl-synthesizer-modules-vca::vca-core))
	   (core-lin (getf core :linear))
	   (core-exp (getf core :exponential)))
      (assert-equal 1.0 (funcall core-lin 10.0))
      (let ((exp (funcall core-exp 10.0)))
	(assert (>= exp 0.99)))
      ))

(define-test test-vca-core-2 ()
    (let* (
	   (core (cl-synthesizer-modules-vca::vca-core))
	   (core-lin (getf core :linear))
	   (core-exp (getf core :exponential)))
      (assert-equal 0.0 (funcall core-lin 0.0))
      (assert-equal 0.0 (funcall core-exp 0.0))
      ))

(define-test test-vca-core-3 ()
    (let* (
	   (core (cl-synthesizer-modules-vca::vca-core))
	   (core-lin (getf core :linear))
	   (core-exp (getf core :exponential)))
      (expect-invalid-arguments-exception
	(funcall core-lin -1.0))
      ))

(define-test test-vca-core-4 ()
    (let* (
	   (core (cl-synthesizer-modules-vca::vca-core))
	   (core-lin (getf core :linear))
	   (core-exp (getf core :exponential)))
      (expect-invalid-arguments-exception
	(funcall core-lin 11.0))
      ))

#|
;; test output when max ampl is being requested
(define-test vca-test-1 ()
	     (let ((vca (cl-synthesizer-modules-vca::vca-ng
			 "VCA"
			 (cl-synthesizer:make-environment)
			 :cv-max 5.0)))
	       (funcall (getf vca :update)
			:input 2.5
			:cv 5.0
			:cv-gain 0.0)
	       (assert-equality
		#'= 2.5
		(float (funcall (getf vca :get-output) :output-exponential)))
	       (assert-equality
		#'= 2.5
		(float (funcall (getf vca :get-output) :output-linear)))

	       ))

      
   
|#

