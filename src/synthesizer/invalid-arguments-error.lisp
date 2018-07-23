(in-package :cl-synthesizer)

(define-condition invalid-arguments-error (error) ())

(defun signal-invalid-arguments-error (&key format-control format-arguments)
  ;;(format t "~%Invalid arguments error: ~a ~a" format-control format-arguments)
  (error (make-condition
	  'invalid-arguments-error
	  :format-control format-control
	  :format-arguments format-arguments)))

