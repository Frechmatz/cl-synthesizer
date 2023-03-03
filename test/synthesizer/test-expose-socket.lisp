(in-package :cl-synthesizer-test)

(define-test test-expose-socket-simple ()
  "Simple exposing"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (let ((m1 (cl-synthesizer:add-module
	       rack
	       "Module 1"
	       #'cl-synthesizer-test::pass-through-module)))
      (cl-synthesizer:add-rack-input rack :rack-input-socket-1 "Module 1" :input-1)
      (cl-synthesizer:add-rack-input rack :rack-input-socket-2 "Module 1" :input-2)
      (cl-synthesizer:add-rack-output rack :rack-output-socket-1 "Module 1" :output-1)
      (cl-synthesizer:add-rack-output rack :rack-output-socket-2 "Module 1" :output-2)
      (assert-true (cl-synthesizer-test::get-rack-output rack :rack-output-socket-1))
      (assert-true (cl-synthesizer-test::get-rack-output rack :rack-output-socket-2))
      (assert-false (cl-synthesizer-test::get-rack-output rack :rack-output-socket-3))
      (assert-true (cl-synthesizer-test::get-rack-input rack :rack-input-socket-1))
      (assert-true (cl-synthesizer-test::get-rack-input rack :rack-input-socket-2))
      (assert-false (cl-synthesizer-test::get-rack-input rack :rack-input-socket-3)))))

(define-test test-expose-input-socket-twice ()
  "Expose same input-socket two times"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (let ((m1 (cl-synthesizer:add-module
	       rack
	       "Module 1"
	       #'cl-synthesizer-test::pass-through-module)))
      (cl-synthesizer:add-rack-input rack :rack-input-socket-1 "Module 1" :input-1)
      (assert-true (cl-synthesizer-test::get-rack-input rack :rack-input-socket-1))
      (expect-assembly-error
	(cl-synthesizer:add-rack-input rack :rack-input-socket-1 "Module 1" :input-1)))))

(define-test test-expose-output-socket-twice ()
  "Expose same output-socket two times"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (let ((m1 (cl-synthesizer:add-module
	       rack
	       "Module 1"
	       #'cl-synthesizer-test::pass-through-module)))
      (cl-synthesizer:add-rack-output rack :rack-output-socket-1 "Module 1" :output-1)
      (assert-true (cl-synthesizer-test::get-rack-output rack :rack-output-socket-1))
      (expect-assembly-error
	(cl-synthesizer:add-rack-output rack :rack-output-socket-1 "Module 1" :output-1)))))

(define-test test-expose-socket-unknown-input-module ()
  "Expose input socket of unknown module"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (let ((m1 (cl-synthesizer:add-module
	       rack
	       "Module 1"
	       #'cl-synthesizer-test::pass-through-module)))
      (expect-assembly-error
	(cl-synthesizer:add-rack-input rack :rack-input-socket-1 "Module 2" :input-1)))))

(define-test test-expose-socket-unknown-output-module ()
  "Expose output socket of unknown module"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (let ((m1 (cl-synthesizer:add-module
	       rack
	       "Module 1"
	       #'cl-synthesizer-test::pass-through-module)))
      (expect-assembly-error
	(cl-synthesizer:add-rack-output rack :rack-output-socket-1 "Module 2" :output-1)))))

(define-test test-expose-socket-unknown-input-module-socket ()
  "Expose input socket of a module that does not have the given input socket"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (let ((m1 (cl-synthesizer:add-module
	       rack
	       "Module 1"
	       #'cl-synthesizer-test::pass-through-module)))
      (expect-assembly-error
	(cl-synthesizer:add-rack-input rack :rack-input-socket-1 "Module 1" :XXXXX)))))

(define-test test-expose-socket-unknown-output-module-socket ()
  "Expose output socket of a module that does not have the given output socket"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (let ((m1 (cl-synthesizer:add-module
	       rack
	       "Module 1"
	       #'cl-synthesizer-test::pass-through-module)))
      (expect-assembly-error
	(cl-synthesizer:add-rack-output rack :rack-output-socket-1 "Module 1" :XXXXX)))))

(define-test test-expose-already-patched-input-socket ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (let ((m1 (cl-synthesizer:add-module
	       rack
	       "Module 1"
	       #'cl-synthesizer-test::pass-through-module))
	  (m2 (cl-synthesizer:add-module
	       rack
	       "Module 2"
	       #'cl-synthesizer-test::pass-through-module)))
      (cl-synthesizer:add-patch rack "Module 1" :output-1 "Module 2" :input-1)
      (expect-assembly-error
	(cl-synthesizer:add-rack-input rack :rack-input-socket-1 "Module 2" :input-1)))))

(define-test test-patch-already-exposed-input-socket ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (let ((m1 (cl-synthesizer:add-module
	       rack
	       "Module 1"
	       #'cl-synthesizer-test::pass-through-module))
	  (m2 (cl-synthesizer:add-module
	       rack
	       "Module 2"
	       #'cl-synthesizer-test::pass-through-module)))
      (cl-synthesizer:add-rack-input rack :rack-input-socket-1 "Module 2" :input-1)
      (expect-assembly-error
	(cl-synthesizer:add-patch rack "Module 1" :output-1 "Module 2" :input-1)))))

(define-test test-expose-already-patched-output-socket ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (let ((m1 (cl-synthesizer:add-module
	       rack
	       "Module 1"
	       #'cl-synthesizer-test::pass-through-module))
	  (m2 (cl-synthesizer:add-module
	       rack
	       "Module 2"
	       #'cl-synthesizer-test::pass-through-module)))
      (cl-synthesizer:add-patch rack "Module 2" :output-1 "Module 1" :input-1)
      (expect-assembly-error
	(cl-synthesizer:add-rack-output rack :rack-output-socket-1 "Module 2" :output-1)))))

(define-test test-patch-already-exposed-output-socket ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (let ((m1 (cl-synthesizer:add-module
	       rack
	       "Module 1"
	       #'cl-synthesizer-test::pass-through-module))
	  (m2 (cl-synthesizer:add-module
	       rack
	       "Module 2"
	       #'cl-synthesizer-test::pass-through-module)))
      (cl-synthesizer:add-rack-output rack :rack-output-socket-1 "Module 1" :output-1)
      (expect-assembly-error
	(cl-synthesizer:add-patch rack "Module 1" :output-1 "Module 2" :input-1)))))

(define-test test-expose-input-socket-unknown-module ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (expect-assembly-error
      (cl-synthesizer:add-rack-input rack :rack-input-socket-1 "Module 1" :input-1))))

(define-test test-expose-input-socket-unknown-input ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (let ((m1 (cl-synthesizer:add-module
	       rack
	       "Module 1"
	       #'cl-synthesizer-test::pass-through-module)))
      (expect-assembly-error
	(cl-synthesizer:add-rack-input rack :rack-input-socket-1 "Module 1" :xxxxxx)))))

(define-test test-expose-output-socket-unknown-module ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (expect-assembly-error
      (cl-synthesizer:add-rack-output rack :rack-output-socket-1 "XXXXX" :output-1))))

(define-test test-expose-output-socket-unknown-output ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (let ((m1 (cl-synthesizer:add-module
	       rack
	       "Module 1"
	       #'cl-synthesizer-test::pass-through-module)))
      (expect-assembly-error
	(cl-synthesizer:add-rack-output rack :rack-output-socket-1 "Module 1" :xxxxxx)))))

