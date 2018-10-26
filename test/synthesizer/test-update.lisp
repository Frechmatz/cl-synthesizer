
(in-package :cl-synthesizer-test)

(defun create-test-rack-simple ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Counter" #'cl-synthesizer-test::test-module-counter)
    (cl-synthesizer:add-module rack "Multiplier" #'cl-synthesizer-test::test-module-multiply-by-two)
    ;; plus 2 default modules of the rack
    (assert-eq 6 (length (getf rack :modules)))
    (let ((found-module-1 (cl-synthesizer::get-rm-module rack "Counter"))
	  (found-module-2 (cl-synthesizer::get-rm-module rack "Multiplier")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-equal "Counter" (cl-synthesizer::get-rack-module-name found-module-1))
      (assert-equal "Multiplier" (cl-synthesizer::get-rack-module-name found-module-2)))
    rack))

(define-test test-rack-update-1 ()
	     (let ((rack (create-test-rack-simple)))
	       (let ((counter (cl-synthesizer::get-rm-module rack "Counter"))
		     (multiplier (cl-synthesizer::get-rm-module rack "Multiplier")))
		 (cl-synthesizer:add-patch
		  rack
		  "Counter" :out
		  "Multiplier" :in)
		 (assert-equal 0 (apply (cl-synthesizer::get-rack-module-output-fn counter) (list :out)))
		 (assert-equal 0 (apply (cl-synthesizer::get-rack-module-output-fn multiplier) (list :out)))
		 (funcall (getf rack :update) nil)
		 (assert-equal 1 (apply (cl-synthesizer::get-rack-module-output-fn counter) (list :out)))
		 (assert-equal 2 (apply (cl-synthesizer::get-rack-module-output-fn multiplier) (list :out)))
		 (funcall (getf rack :update) nil)
		 (assert-equal 2 (apply (cl-synthesizer::get-rack-module-output-fn counter) (list :out)))
		 (assert-equal 4 (apply (cl-synthesizer::get-rack-module-output-fn multiplier) (list :out)))
		 )))


(defun create-test-rack-adder ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Counter 1" #'cl-synthesizer-test::test-module-counter)
    (cl-synthesizer:add-module rack "Counter 2" #'cl-synthesizer-test::test-module-counter)
    (cl-synthesizer:add-module rack "Adder" #'cl-synthesizer-test::test-module-adder)
    ;; plus 2 default modules of the rack
    (assert-eq 7 (length (getf rack :modules)))
    (let ((found-module-1 (cl-synthesizer::get-rm-module rack "Counter 1"))
	  (found-module-2 (cl-synthesizer::get-rm-module rack "Counter 2"))
	  (found-module-3 (cl-synthesizer::get-rm-module rack "Adder")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-true found-module-3)
      (assert-equal "Counter 1" (cl-synthesizer::get-rack-module-name found-module-1))
	(assert-equal "Counter 2" (cl-synthesizer::get-rack-module-name found-module-2))
	(assert-equal "Adder" (cl-synthesizer::get-rack-module-name found-module-3)))
    rack))

(define-test test-rack-update-adder ()
	     (let ((rack (create-test-rack-adder)))
	       (let ((counter-1 (cl-synthesizer::get-rm-module rack "Counter 1"))
		     (counter-2 (cl-synthesizer::get-rm-module rack "Counter 2"))
		     (adder (cl-synthesizer::get-rm-module rack "Adder")))
		 (cl-synthesizer:add-patch
		  rack
		  "Counter 1" :out
		  "Adder" :in-1)
		 (cl-synthesizer:add-patch
		  rack
		  "Counter 2" :out
		  "Adder" :in-2)
		 (assert-equal 0 (apply (cl-synthesizer::get-rack-module-output-fn counter-1) (list :out)))
		 (assert-equal 0 (apply (cl-synthesizer::get-rack-module-output-fn counter-2) (list :out)))
		 (assert-equal 0 (apply (cl-synthesizer::get-rack-module-output-fn adder) (list :out)))
		 (funcall (getf rack :update) nil)
		 (assert-equal 1 (apply (cl-synthesizer::get-rack-module-output-fn counter-1) (list :out)))
		 (assert-equal 1 (apply (cl-synthesizer::get-rack-module-output-fn counter-2) (list :out)))
		 (assert-equal 2 (apply (cl-synthesizer::get-rack-module-output-fn adder) (list :out)))
		 (funcall (getf rack :update) nil)
		 (assert-equal 2 (apply (cl-synthesizer::get-rack-module-output-fn counter-1) (list :out)))
		 (assert-equal 2 (apply (cl-synthesizer::get-rack-module-output-fn counter-2) (list :out)))
		 (assert-equal 4 (apply (cl-synthesizer::get-rack-module-output-fn adder) (list :out)))
		 )))

(defun create-test-rack-recursive ()
  "Input of adder is connected with one of its outputs"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Counter" #'cl-synthesizer-test::test-module-counter)
    (cl-synthesizer:add-module rack "Adder" #'cl-synthesizer-test::test-module-adder)
    ;; plus 2 default modules of the rack
    (assert-eq 6 (length (getf rack :modules)))
    (let ((found-module-1 (cl-synthesizer::get-rm-module rack "Counter"))
	  (found-module-2 (cl-synthesizer::get-rm-module rack "Adder")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-equal "Counter" (cl-synthesizer::get-rack-module-name found-module-1))
      (assert-equal "Adder" (cl-synthesizer::get-rack-module-name found-module-2)))
    rack))

(define-test test-rack-update-recursive ()
	     (let ((rack (create-test-rack-recursive)))
	       (let ((counter (cl-synthesizer::get-rm-module rack "Counter"))
		     (adder (cl-synthesizer::get-rm-module rack "Adder")))
		 (cl-synthesizer:add-patch
		  rack
		  "Counter" :out
		  "Adder" :in-1)
		 (cl-synthesizer:add-patch
		  rack
		  "Adder" :out
		  "Adder" :in-2)
		 (assert-equal 0 (apply (cl-synthesizer::get-rack-module-output-fn counter) (list :out)))
		 (assert-equal 0 (apply (cl-synthesizer::get-rack-module-output-fn adder) (list :out)))
		 (funcall (getf rack :update) nil)
		 (assert-equal 1 (apply (cl-synthesizer::get-rack-module-output-fn counter) (list :out)))
		 (assert-equal 1 (apply (cl-synthesizer::get-rack-module-output-fn adder) (list :out)))
		 (funcall (getf rack :update) nil)
		 (assert-equal 2 (apply (cl-synthesizer::get-rack-module-output-fn counter) (list :out)))
		 (assert-equal 3 (apply (cl-synthesizer::get-rack-module-output-fn adder) (list :out)))
		 (funcall (getf rack :update) nil)
		 (assert-equal 3 (apply (cl-synthesizer::get-rack-module-output-fn counter) (list :out)))
		 (assert-equal 6 (apply (cl-synthesizer::get-rack-module-output-fn adder) (list :out)))
		 )))
