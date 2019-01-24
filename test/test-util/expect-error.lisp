(in-package :cl-synthesizer-test)


(defmacro expect-assembly-error(&body body)
  `(handler-case
       (progn
         ,@body
         (assert-true nil))
     (cl-synthesizer::assembly-error (err)
       (assert-true err) ;; increase test count of lisp-unit summary
       t)
     (error (err)
       (assert-false err)
       nil)))

(defmacro expect-invalid-arguments-error(&body body)
  `(handler-case
       (progn
         ,@body
         (assert-true nil))
     (cl-synthesizer::invalid-arguments-error (err)
       (assert-true err) ;; increase test count of lisp-unit summary
       t)
     (error (err)
       (assert-false err)
       nil)))

