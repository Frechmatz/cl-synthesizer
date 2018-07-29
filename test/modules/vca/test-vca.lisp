(in-package :cl-synthesizer-test)

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



