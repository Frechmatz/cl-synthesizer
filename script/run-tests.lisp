
;;; Initialize quicklisp (copied from .sbclrc)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:load-system "cl-synthesizer-test" :force t)
(in-package :cl-synthesizer-test)
(format t "~%Running tests...~%")
(setf lisp-unit:*print-failures* t)
;;(use-debugger)
(run-tests)

