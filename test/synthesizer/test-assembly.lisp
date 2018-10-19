
(in-package :cl-synthesizer-test)

(define-test test-add-module-to-rack-1 ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Module 1" #'cl-synthesizer-test::test-module)
    ;; Module 1 plus 2 default modules of the rack
    (assert-eq 3 (length (getf rack :modules)))
    (assert-true (cl-synthesizer::get-rm-module rack "Module 1"))))

(define-test test-add-module-to-rack-2 ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Module 1" #'cl-synthesizer-test::test-module)
    (cl-synthesizer:add-module rack "Module 2" #'cl-synthesizer-test::test-module)
    ;; plus 2 default modules of the rack
    (assert-eq 4 (length (getf rack :modules)))
    (let ((found-module-1 (cl-synthesizer::get-rm-module rack "Module 1"))
	  (found-module-2 (cl-synthesizer::get-rm-module rack "Module 2")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-equal "Module 1" (cl-synthesizer::get-rack-module-name found-module-1))
      (assert-equal "Module 2" (cl-synthesizer::get-rack-module-name found-module-2))
      )))

(define-test test-add-module-to-rack-already-exists ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Module 1" #'cl-synthesizer-test::test-module)
    (expect-assembly-exception
      (cl-synthesizer:add-module rack "Module 1" #'cl-synthesizer-test::test-module))
    ;; plus 2 default modules of the rack
    (assert-eq 3 (length (getf rack :modules)))
    (let ((found-module-1 (cl-synthesizer::get-rm-module rack "Module 1"))
	  (found-module-2 (cl-synthesizer::get-rm-module rack "Module 2")))
      (assert-true found-module-1)
      (assert-false found-module-2)
      (assert-equal "Module 1" (cl-synthesizer::get-rack-module-name found-module-1))
      )))

