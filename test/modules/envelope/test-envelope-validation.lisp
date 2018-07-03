(in-package :cl-synthesizer-test)

(defun envelope-validation-test-instantiate-module (segments)
  (let ((module (cl-synthesizer-modules-envelope:envelope
		 "Envelope Module"
		 (cl-synthesizer:make-environment)
		 :segments segments)))
    nil))

(define-test test-envelope-validation-1 ()
	     (envelope-validation-test-instantiate-module
	      '((:duration-ms 50 :target-cv 5 :required-gate-state :on)))
	     (assert-equal 1 1))

(define-test test-envelope-validation-2 ()
	     (expect-assembly-exception
	       (envelope-validation-test-instantiate-module
		'((:duration-ms nil :target-cv 5 :required-gate-state :ignore)))))

(define-test test-envelope-validation-3 ()
	     (expect-assembly-exception
	       (envelope-validation-test-instantiate-module
		'((:duration-ms nil :target-cv 5 :required-gate-state :ignore)))))

(define-test test-envelope-validation-4 ()
	     (envelope-validation-test-instantiate-module
	      '((:duration-ms 50 :target-cv 5 :required-gate-state :on
		 :duration-controller (:socket :a
				       :input-min 1
				       :input-max 1
				       :output-min 1
				       :output-max 1
				       ))))
	     (assert-equal 1 1))

(define-test test-envelope-validation-5 ()
	     (expect-assembly-exception
	       (envelope-validation-test-instantiate-module
		'((:duration-ms nil :target-cv 5 :required-gate-state :on
		   :duration-controller (:socket :a
				       :input-min 1
				       :input-max 1
				       :output-min 1
				       :output-max 1))))))

(define-test test-envelope-validation-6 ()
	     (envelope-validation-test-instantiate-module
	      '((:duration-ms 50 :target-cv 5 :required-gate-state :on
		 :target-cv-controller (:socket :a
				       :input-min 1
				       :input-max 1
				       :output-min 1
				       :output-max 1))))
	     (assert-equal 1 1))

(define-test test-envelope-validation-7 ()
	     (expect-assembly-exception
	       (envelope-validation-test-instantiate-module
		'((:duration-ms nil :target-cv nil :required-gate-state :on
		   :target-cv-controller (:socket :a
				       :input-min 1
				       :input-max 1
				       :output-min 1
				       :output-max 1))))))

(define-test test-envelope-validation-8 ()
	     (expect-assembly-exception
	       (envelope-validation-test-instantiate-module
		'((:duration-ms 5 :target-cv 5 :required-gate-state :on
		   :target-cv-controller (:socket :DUPLICATE :input-min 1 :input-max 1 :output-min 1 :output-max 1))
		  (:duration-ms nil :target-cv 5 :required-gate-state :on
		   :target-cv-controller (:socket :DUPLICATE :input-min 1 :input-max 1 :output-min 1 :output-max 1)))
		  )))

(define-test test-envelope-validation-8-1 ()
	     (envelope-validation-test-instantiate-module
	      '((:duration-ms 5 :target-cv 5 :required-gate-state :on
		 :target-cv-controller (:socket :SOCKET-1 :input-min 1 :input-max 1 :output-min 1 :output-max 1))
		(:duration-ms nil :target-cv 5 :required-gate-state :on
		 :target-cv-controller (:socket :SOCKET-2 :input-min 1 :input-max 1 :output-min 1 :output-max 1))))
	     (assert-equal 1 1))

(define-test test-envelope-validation-9 ()
	     (expect-assembly-exception
	       (envelope-validation-test-instantiate-module
		'((:duration-ms 5 :target-cv 5 :required-gate-state :on
		   :target-cv-controller (:socket :DUPLICATE :input-min 1 :input-max 1 :output-min 1 :output-max 1)
		   :duration-controller (:socket :DUPLICATE :input-min 1 :input-max 1 :output-min 1 :output-max 1)))
		  )))

(define-test test-envelope-validation-9-1 ()
	     (envelope-validation-test-instantiate-module
	      '((:duration-ms 5 :target-cv 5 :required-gate-state :on
		 :target-cv-controller (:socket :SOCKET-1 :input-min 1 :input-max 1 :output-min 1 :output-max 1)
		 :duration-controller (:socket :SOCKET-2 :input-min 1 :input-max 1 :output-min 1 :output-max 1))))
	     (assert-equal 1 1))

(define-test test-envelope-validation-reserved-keyword-1 ()
	     (expect-assembly-exception
	       (envelope-validation-test-instantiate-module
		'((:duration-ms 5 :target-cv 5 :required-gate-state :on
		   :target-cv-controller (:socket :gate :input-min 1 :input-max 1 :output-min 1 :output-max 1)))
		  )))

(define-test test-envelope-validation-reserved-keyword-2 ()
	     (expect-assembly-exception
	       (envelope-validation-test-instantiate-module
		'((:duration-ms 5 :target-cv 5 :required-gate-state :on
		   :duration-controller (:socket :gate :input-min 1 :input-max 1 :output-min 1 :output-max 1)))
		  )))
