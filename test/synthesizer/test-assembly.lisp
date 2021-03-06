
(in-package :cl-synthesizer-test)

(define-test test-add-module-to-rack-1 ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (let ((m1 (cl-synthesizer:add-module rack "Module 1" #'cl-synthesizer-test::pass-through-module)))
      ;; Module 1 plus 2 default modules of the rack
      (assert-eq 3 (length (cl-synthesizer:get-modules rack)))
      (assert-true (cl-synthesizer:get-module rack "Module 1"))
      (assert-eq m1 (cl-synthesizer:get-module rack "Module 1")))))

(define-test test-add-module-to-rack-2 ()
    (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
      (let ((m1 (cl-synthesizer:add-module rack "Module 1" #'cl-synthesizer-test::pass-through-module))
	    (m2 (cl-synthesizer:add-module rack "Module 2" #'cl-synthesizer-test::pass-through-module)))
    ;; plus 2 default modules of the rack
    (assert-eq 4 (length (cl-synthesizer:get-modules rack)))
    (let ((found-module-1 (cl-synthesizer:get-module rack "Module 1"))
	  (found-module-2 (cl-synthesizer:get-module rack "Module 2")))
      (assert-true found-module-1)
      (assert-eq m1 found-module-1)
      (assert-true found-module-2)
      (assert-eq m2 found-module-2)
      (assert-equal "Module 1" (cl-synthesizer:get-module-name rack found-module-1))
      (assert-equal "Module 2" (cl-synthesizer:get-module-name rack found-module-2))))))

(define-test test-add-module-to-rack-already-exists ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Module 1" #'cl-synthesizer-test::pass-through-module)
    (expect-assembly-error
      (cl-synthesizer:add-module rack "Module 1" #'cl-synthesizer-test::pass-through-module))
    ;; plus 2 default modules of the rack
    (assert-eq 3 (length (cl-synthesizer:get-modules rack)))
    (let ((found-module-1 (cl-synthesizer:get-module rack "Module 1"))
	  (found-module-2 (cl-synthesizer:get-module rack "Module 2")))
      (assert-true found-module-1)
      (assert-false found-module-2)
      (assert-equal "Module 1" (cl-synthesizer:get-module-name rack found-module-1)))))
