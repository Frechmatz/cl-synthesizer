(in-package :cl-synthesizer-test)


(defmacro expect-assembly-error(&body body)
  "Fail test if body does not signal an assembly-error."
  `(handler-case
       (progn
         ,@body
         (assert-true nil))
     (cl-synthesizer:assembly-error (err)
       ;;(format t "~%Catched expected assembly-error: ~a~%" err)
       (assert-true err) ;; increase test count of lisp-unit summary
       t)
     (error (err)
       (format t "~%Catched unexpected error: ~a~%" err)
       (assert-false err)
       nil)))

(defmacro expect-simple-error(&body body)
  "Fail test if body does not signal a simple-error."
  `(handler-case
       (progn
         ,@body
         (assert-true nil))
     (simple-error (err)
       ;;(format t "~%Catched expected simple-error: ~a~%" err)
       (assert-true err) ;; increase test count of lisp-unit summary
       t)
     (error (err)
       (format t "~%Catched unexpected error: ~a~%" err)
       (assert-false err)
       nil)))

