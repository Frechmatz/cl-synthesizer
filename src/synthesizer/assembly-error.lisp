(in-package :cl-synthesizer)

(define-condition assembly-error (error) ())

(defun signal-assembly-error (&key format-control format-arguments)
  ;;(break)
  (error (make-condition
	  'assembly-error
	  :format-control format-control
	  :format-arguments format-arguments)))

