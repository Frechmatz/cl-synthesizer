(in-package :cl-synthesizer)

(define-condition invalid-arguments-error (error) () (:documentation "XX"))

(defun signal-invalid-arguments-error (&key format-control format-arguments)
  ;;(format t "~%Invalid arguments error: ~a ~a" format-control format-arguments)
  (error (make-condition
	  'invalid-arguments-error
	  :format-control format-control
	  :format-arguments format-arguments)))

(define-condition assembly-error (error) ()
  (:documentation
   "This condition is signalled in cases where the assembly of a rack fails,
   because for example a module name is not unique, a patch is added for
   a non-existing module, a patch is added to an already patched socket
   and so on."))

(defun signal-assembly-error (&key format-control format-arguments)
  ;;(format t "~%Assembly error: ~a ~a" format-control format-arguments)
  (error (make-condition
	  'assembly-error
	  :format-control format-control
	  :format-arguments format-arguments)))
