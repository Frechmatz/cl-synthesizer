(load "init-ql")
(asdf:load-system :cl-synthesizer/doc :force t)
(cl-synthesizer-make-doc::make-doc)
