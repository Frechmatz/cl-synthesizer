(in-package :cl-synthesizer-test)

(define-test test-adder-1 ()
    (let ((adder (cl-synthesizer-modules-adder:make-module "Adder" (cl-synthesizer:make-environment) :input-count 2)))
      (update-module adder (list :input-1 1.0 :input-2 2.0))
      (assert-equal 3.0 (get-module-output adder :output))))

(define-test test-adder-2 ()
    (let ((adder (cl-synthesizer-modules-adder:make-module "Adder" (cl-synthesizer:make-environment) :input-count 2)))
      (update-module adder (list :input-1 -1.0 :input-2 5))
      (assert-equal 4.0 (get-module-output adder :output))))

(define-test test-adder-3 ()
    (let ((adder (cl-synthesizer-modules-adder:make-module "Adder" (cl-synthesizer:make-environment) :input-count 2)))
      (update-module adder (list :input-1 -1 :input-2 5))
      (assert-equal 4 (get-module-output adder :output))))

(define-test test-adder-4 ()
    (let ((adder (cl-synthesizer-modules-adder:make-module "Adder" (cl-synthesizer:make-environment) :input-count 2)))
      (update-module adder (list :input-1 nil :input-2 5.0))
      (assert-equal 5.0 (get-module-output adder :output))))

(define-test test-adder-5 ()
    (let ((adder (cl-synthesizer-modules-adder:make-module "Adder" (cl-synthesizer:make-environment) :input-count 2)))
      (update-module adder (list :input-1 "100" :input-2 5.0))
      (assert-equal 5.0 (get-module-output adder :output))))

