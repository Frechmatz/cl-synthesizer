;;
;; How to run:
;;
;; (asdf:load-system :cl-synthesizer-examples)
;; (cl-synthesizer-examples::run-examples)
;;

(defpackage :cl-synthesizer-examples
  (:use :cl))

(in-package :cl-synthesizer-examples)

(defun run-examples ()
  (format t "~%Running examples....~%")
  (finish-output)
  ;; For all packages whose name begin with "CL-SYNTHESIZER-" and
  ;; that expose a function "run-example"
  (dolist (p (list-all-packages))
    ;;(format t "~%Checking package ~a" (package-name p))
    (let ((pos (search "CL-SYNTHESIZER-" (package-name p))))
      (if (eql 0 pos)
	  (let ((run-example-symbol (find-symbol "RUN-EXAMPLE" p)))
	    (if run-example-symbol
		(progn
		  (format t "~%Running example ~a::~a" (package-name p) (symbol-name run-example-symbol))
		  (finish-output)
		  (funcall (symbol-function run-example-symbol))
		  (format t "~%Example ~a::~a has completed" (package-name p) (symbol-name run-example-symbol))
		  (finish-output)))))))
  (finish-output)
  (format t "~%Examples have completed~%")
  (finish-output))

;; (run-examples)
