(in-package :cl-synthesizer-test)

(define-test test-cc-mapper-1 ()
  (let ((mapper (cl-synthesizer-midi::cc-mapper)))
    (funcall (getf mapper :add-mapping) 64 1 1000)
    (funcall (getf mapper :add-mapping) 64 2 1001)
    (funcall (getf mapper :add-mapping) 64 3 1002)
    (assert-equal 1000 (funcall (getf mapper :map) 64 1))
    (assert-equal 1001 (funcall (getf mapper :map) 64 2))
    (assert-equal 1002 (funcall (getf mapper :map) 64 3))
    (assert-false (funcall (getf mapper :map) 64 4))))

(define-test test-cc-mapper-2 ()
  (let ((mapper (cl-synthesizer-midi::cc-mapper)))
    (funcall (getf mapper :add-list-mapping) 64 '(1 2 3) 1000)
    (funcall (getf mapper :add-list-mapping) 64 '(10 20 30) 10000)
    (assert-equal 1000 (funcall (getf mapper :map) 64 1))
    (assert-equal 1000 (funcall (getf mapper :map) 64 2))
    (assert-equal 1000 (funcall (getf mapper :map) 64 3))
    (assert-equal 10000 (funcall (getf mapper :map) 64 10))
    (assert-equal 10000 (funcall (getf mapper :map) 64 20))
    (assert-equal 10000 (funcall (getf mapper :map) 64 30))
    (assert-false (funcall (getf mapper :map) 64 4))))

(define-test test-cc-mapper-3 ()
  (let ((mapper (cl-synthesizer-midi::cc-mapper)))
    (funcall (getf mapper :add-range-mapping) 64 1 3 1000)
    (funcall (getf mapper :add-range-mapping) 64 11 13 10000)
    (assert-equal 1000 (funcall (getf mapper :map) 64 1))
    (assert-equal 1000 (funcall (getf mapper :map) 64 2))
    (assert-equal 1000 (funcall (getf mapper :map) 64 3))
    (assert-equal 10000 (funcall (getf mapper :map) 64 11))
    (assert-equal 10000 (funcall (getf mapper :map) 64 12))
    (assert-equal 10000 (funcall (getf mapper :map) 64 13))
    (assert-false (funcall (getf mapper :map) 64 4))))

(define-test test-cc-mapper-4 ()
  (let ((mapper (cl-synthesizer-midi::cc-mapper)))
    (funcall (getf mapper :add-mapping) 64 1 999)
    (expect-assembly-error
     (funcall (getf mapper :add-mapping) 64 1 999))))

(define-test test-cc-mapper-5 ()
  (let ((mapper (cl-synthesizer-midi::cc-mapper)))
    (funcall (getf mapper :add-mapping) 64 5 999)
    (expect-assembly-error
     (funcall (getf mapper :add-list-mapping) 64 '(1 2 3 4 5) 999))))

(define-test test-cc-mapper-6 ()
  (let ((mapper (cl-synthesizer-midi::cc-mapper)))
    (funcall (getf mapper :add-mapping) 64 5 999)
    (expect-assembly-error
     (funcall (getf mapper :add-range-mapping) 64 4 10 999))))

(define-test test-cc-mapper-7 ()
  (let ((mapper (cl-synthesizer-midi::cc-mapper)))
    (funcall (getf mapper :add-range-mapping) 64 5 10 999)
    (expect-assembly-error
     (funcall (getf mapper :add-range-mapping) 64 4 10 999))))

(define-test test-cc-mapper-8 ()
  (let ((mapper (cl-synthesizer-midi::cc-mapper)))
    (funcall (getf mapper :add-list-mapping) 64 '(4 5 6 7) 999)
    (expect-assembly-error
     (funcall (getf mapper :add-list-mapping) 64 '(6 10 11) 999))))
