(in-package :cl-synthesizer-test)

(define-test test-adder-1 ()
    (let ((adder (cl-synthesizer-modules-adder:adder "Adder" (cl-synthesizer:make-environment) :input-count 2)))
      (funcall (getf adder :update) :input-1 1.0 :input-2 2.0)
      (assert-equal 3.0 (funcall (getf adder :get-output) :output))))

(define-test test-adder-2 ()
    (let ((adder (cl-synthesizer-modules-adder:adder "Adder" (cl-synthesizer:make-environment) :input-count 2)))
      (funcall (getf adder :update) :input-1 -1.0 :input-2 5)
      (assert-equal 4.0 (funcall (getf adder :get-output) :output))))

(define-test test-adder-3 ()
    (let ((adder (cl-synthesizer-modules-adder:adder "Adder" (cl-synthesizer:make-environment) :input-count 2)))
      (funcall (getf adder :update) :input-1 -1 :input-2 5)
      (assert-equal 4 (funcall (getf adder :get-output) :output))))

(define-test test-adder-4 ()
    (let ((adder (cl-synthesizer-modules-adder:adder "Adder" (cl-synthesizer:make-environment) :input-count 2)))
      (funcall (getf adder :update) :input-1 nil :input-2 5.0)
      (assert-equal 5.0 (funcall (getf adder :get-output) :output))))

(define-test test-adder-5 ()
    (let ((adder (cl-synthesizer-modules-adder:adder "Adder" (cl-synthesizer:make-environment) :input-count 2)))
      (funcall (getf adder :update) :input-1 "100" :input-2 5.0)
      (assert-equal 5.0 (funcall (getf adder :get-output) :output))))

