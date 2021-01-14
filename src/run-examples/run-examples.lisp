(in-package :cl-synthesizer-examples)

(defun run-examples ()
  "Run all examples of cl-synthesizer. Examples are defined as follows:
   <ul>
   <li>The package name begins with \"cl-synthesizer-\"</li>
   <li>The package has a function \"run-example\"</li>
   </ul>"
  (format t "~%Running examples....~%")
  (finish-output)
  (dolist (p (list-all-packages))
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
