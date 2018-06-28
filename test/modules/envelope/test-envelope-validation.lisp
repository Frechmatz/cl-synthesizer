(in-package :cl-synthesizer-test)

(defun envelope-validation-test-instantiate-module (segments)
  (let ((module (cl-synthesizer-modules-envelope:envelope
		 "Envelope Module"
		 (cl-synthesizer:make-environment)
		 :segments segments)))
    nil))

(define-test test-envelope-validation-1 ()
	     (envelope-validation-test-instantiate-module
	      '((:duration-ticks 50 :target-cv 5 :required-gate-state :on)))
	     (assert-equal 1 1))

(define-test test-envelope-validation-2 ()
	     (expect-assembly-exception
	       (envelope-validation-test-instantiate-module
		'((:duration-ticks nil :target-cv 5 :required-gate-state :ignore)))))

(define-test test-envelope-validation-3 ()
	     (expect-assembly-exception
	       (envelope-validation-test-instantiate-module
		'((:duration-ticks nil :target-cv 5 :required-gate-state :ignore)))))

 
