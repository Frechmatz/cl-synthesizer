(in-package :cl-synthesizer-test)


(defmacro expect-assembly-error(&body body)
  "Fail test if body does not signal an assembly-error."
  `(handler-case
       (progn
         ,@body
         (assert-true nil))
     (cl-synthesizer:assembly-error (err)
       (assert-true err) ;; increase test count of lisp-unit summary
       t)
     (error (err)
       (assert-false err)
       nil)))

(defmacro expect-invalid-arguments-error(&body body)
  "Fail test if body does not signal an invalid-arguments-error."
  `(handler-case
       (progn
         ,@body
         (assert-true nil))
     (cl-synthesizer:invalid-arguments-error (err)
       (assert-true err) ;; increase test count of lisp-unit summary
       t)
     (error (err)
       (assert-false err)
       nil)))

