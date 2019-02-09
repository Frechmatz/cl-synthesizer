(load "init-ql")
(asdf:load-system "cl-synthesizer-test" :force t)
(in-package :cl-synthesizer-test)
(format t "~%Running tests...~%")
(setf lisp-unit:*print-failures* t)
;;(use-debugger)
(run-tests)

