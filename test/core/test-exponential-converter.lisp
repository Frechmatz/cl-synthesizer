(in-package :cl-synthesizer-test)

(define-test test-exponential-converter-1 ()
	     (let ((converter (funcall #'cl-synthesizer-core:exponential-converter :base-value 10)))
	       (let ((conversion-fn (getf converter :get-y)))
		 (assert-equal 10 (funcall conversion-fn 0)))))

(define-test test-exponential-converter-2 ()
	     (let ((converter (funcall #'cl-synthesizer-core:exponential-converter :base-value 10)))
	       (let ((conversion-fn (getf converter :get-y)))
		 (assert-equal 20 (funcall conversion-fn 1)))))

(define-test test-exponential-converter-3 ()
	     (let ((converter (funcall #'cl-synthesizer-core:exponential-converter :base-value 10)))
	       (let ((conversion-fn (getf converter :get-y)))
		 (assert-equal 40 (funcall conversion-fn 2)))))

(define-test test-exponential-converter-4 ()
	     (let ((converter (funcall #'cl-synthesizer-core:exponential-converter :base-value 10)))
	       (let ((conversion-fn (getf converter :get-y)))
		 (assert-equal 5 (funcall conversion-fn -1)))))

