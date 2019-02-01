
(in-package :cl-synthesizer-test)

(defun create-test-rack-simple ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Counter" #'cl-synthesizer-test::update-counter-module)
    (cl-synthesizer:add-module rack "Multiplier" #'cl-synthesizer-test::multiplier-module)
    ;; plus 2 default modules of the rack
    (assert-eq 4 (length (funcall (getf rack :rack-modules))))
    (let ((found-module-1 (funcall (getf rack :get-rack-module-by-name) "Counter"))
	  (found-module-2 (funcall (getf rack :get-rack-module-by-name) "Multiplier")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-equal "Counter" (cl-synthesizer::get-rack-module-name found-module-1))
      (assert-equal "Multiplier" (cl-synthesizer::get-rack-module-name found-module-2)))
    rack))

(define-test test-rack-update-1 ()
	     (let ((rack (create-test-rack-simple)))
	       (let ((counter (funcall (getf rack :get-rack-module-by-name) "Counter"))
		     (multiplier (funcall (getf rack :get-rack-module-by-name) "Multiplier")))
		 (cl-synthesizer:add-patch
		  rack
		  "Counter" :out
		  "Multiplier" :in)
		 (assert-equal 0 (funcall (cl-synthesizer::get-rack-module-output-fn counter) :out))
		 (assert-equal 0 (funcall (cl-synthesizer::get-rack-module-output-fn multiplier) :out))
		 (funcall (getf rack :update) nil)
		 (assert-equal 1 (funcall (cl-synthesizer::get-rack-module-output-fn counter) :out))
		 (assert-equal 2 (funcall (cl-synthesizer::get-rack-module-output-fn multiplier) :out))
		 (funcall (getf rack :update) nil)
		 (assert-equal 2 (funcall (cl-synthesizer::get-rack-module-output-fn counter) :out))
		 (assert-equal 4 (funcall (cl-synthesizer::get-rack-module-output-fn multiplier) :out))
		 )))


(defun create-test-rack-adder ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Counter 1" #'cl-synthesizer-test::update-counter-module)
    (cl-synthesizer:add-module rack "Counter 2" #'cl-synthesizer-test::update-counter-module)
    (cl-synthesizer:add-module rack "Adder" #'cl-synthesizer-test::input-adder-module)
    ;; plus 2 default modules of the rack
    (assert-eq 5 (length (funcall (getf rack :rack-modules))))
    (let ((found-module-1 (funcall (getf rack :get-rack-module-by-name) "Counter 1"))
	  (found-module-2 (funcall (getf rack :get-rack-module-by-name) "Counter 2"))
	  (found-module-3 (funcall (getf rack :get-rack-module-by-name) "Adder")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-true found-module-3)
      (assert-equal "Counter 1" (cl-synthesizer::get-rack-module-name found-module-1))
      (assert-equal "Counter 2" (cl-synthesizer::get-rack-module-name found-module-2))
      (assert-equal "Adder" (cl-synthesizer::get-rack-module-name found-module-3)))
    rack))

(define-test test-rack-update-adder ()
	     (let ((rack (create-test-rack-adder)))
	       (let ((counter-1 (funcall (getf rack :get-rack-module-by-name) "Counter 1"))
		     (counter-2 (funcall (getf rack :get-rack-module-by-name) "Counter 2"))
		     (adder (funcall (getf rack :get-rack-module-by-name) "Adder")))
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
    (cl-synthesizer:add-module rack "Counter" #'cl-synthesizer-test::update-counter-module)
    (cl-synthesizer:add-module rack "Adder" #'cl-synthesizer-test::input-adder-module)
    ;; plus 2 default modules of the rack
    (assert-eq 4 (length (funcall (getf rack :rack-modules))))
    (let ((found-module-1 (funcall (getf rack :get-rack-module-by-name) "Counter"))
	  (found-module-2 (funcall (getf rack :get-rack-module-by-name) "Adder")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-equal "Counter" (cl-synthesizer::get-rack-module-name found-module-1))
      (assert-equal "Adder" (cl-synthesizer::get-rack-module-name found-module-2)))
    rack))

(define-test test-rack-update-recursive ()
	     (let ((rack (create-test-rack-recursive)))
	       (let ((counter (funcall (getf rack :get-rack-module-by-name) "Counter"))
		     (adder (funcall (getf rack :get-rack-module-by-name) "Adder")))
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
