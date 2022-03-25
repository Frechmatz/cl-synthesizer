(in-package :cl-synthesizer-test)

(define-test test-wave-file-writer-1 ()
	     (let ((recorded-samples nil)
		   (open-file-called nil)
		   (close-file-called nil))
	       (let ((cl-synthesizer-modules-wave-file-writer::*make-writer*
		      (lambda (&rest args)
			(declare (ignore args))
			(list
			 :open-file (lambda () (setf open-file-called t))
			 :write-sample (lambda (sample)
					 (push sample recorded-samples))
			 :close-file (lambda () (setf close-file-called t))))))
		 (let ((module (cl-synthesizer-modules-wave-file-writer:make-module
				"Wave-File-Writer"
				(cl-synthesizer:make-environment)
				:channel-count 2 :filename "test" :v-peak 10.0)))
		   (update-module module (list (list :channel-1 1.0) (list :channel-2 2.0)))
		   (update-module module (list (list :channel-1 3.0) (list :channel-2 4.0)))
		   (cl-synthesizer:shutdown module)
		   (setf recorded-samples (reverse recorded-samples))
		   (assert-equal 4 (length recorded-samples))
		   (assert-true (= 0.1 (first recorded-samples)))
		   (assert-true (= 0.2 (second recorded-samples)))
		   (assert-true (= 0.3 (third recorded-samples)))
		   (assert-true (= 0.4 (fourth recorded-samples)))
		   (assert-true open-file-called)
		   (assert-true close-file-called)))))

