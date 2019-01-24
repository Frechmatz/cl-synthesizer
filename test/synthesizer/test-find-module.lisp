
(in-package :cl-synthesizer-test)

(defun test-find-module-create-nested-rack (name environment)
  (declare (ignore name))
  (let ((rack (cl-synthesizer:make-rack :environment environment)))
    (cl-synthesizer:add-module rack "Multiplier" #'cl-synthesizer-test::multiplier-module)
    rack))

(define-test test-find-module-1 ()
	     (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
	       (cl-synthesizer:add-module rack "Multiplier" #'cl-synthesizer-test::multiplier-module)
	       (cl-synthesizer:add-module rack "RACK-1" #'test-find-module-create-nested-rack)
	       (cl-synthesizer:add-module rack "RACK-2" #'test-find-module-create-nested-rack)
	       (cl-synthesizer:add-module rack "RACK-3" #'test-find-module-create-nested-rack)
	       (let ((nested-rack (cl-synthesizer:get-module rack "RACK-3")))
		 (cl-synthesizer:add-module nested-rack "Multiplier 2" #'cl-synthesizer-test::multiplier-module)
		 (cl-synthesizer:add-module nested-rack "RACK-3-1" #'test-find-module-create-nested-rack)
		 (let ((nested-nested-rack (cl-synthesizer:get-module nested-rack "RACK-3-1")))
		   (cl-synthesizer:add-module nested-nested-rack "Multiplier 3" #'cl-synthesizer-test::multiplier-module)))

	       (assert-true (cl-synthesizer:find-module rack "Multiplier"))
	       (assert-true (cl-synthesizer:find-module rack (list "Multiplier")))
	       (assert-false (cl-synthesizer:find-module rack (list "Multiplier" nil)))
	       (assert-false (cl-synthesizer:find-module rack (list "Multiplier" 5.0)))
	       (assert-true (cl-synthesizer:find-module rack "RACK-1"))
	       (assert-true (cl-synthesizer:find-module rack (list "RACK-1")))
	       (assert-true (cl-synthesizer:find-module rack (list "RACK-1" "Multiplier")))
	       (assert-false (cl-synthesizer:find-module rack (list "RACK-1" "Unknown module")))
	       (assert-false (cl-synthesizer:find-module rack (list "RACK-1" "Multiplier 2")))
	       (assert-true (cl-synthesizer:find-module rack (list "RACK-3" "Multiplier 2")))
	       (assert-true (cl-synthesizer:find-module rack (list "RACK-3" "RACK-3-1" "Multiplier 3")))))


