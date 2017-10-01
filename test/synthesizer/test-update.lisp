
(in-package :cl-synthesizer-test)

(defun create-test-rack-simple ()
  (let ((rack (make-instance 'cl-synthesizer:rack)))
    (let ((rm1 (cl-synthesizer::make-rack-module "Counter" #'cl-synthesizer-test::test-module-counter))
	  (rm2 (cl-synthesizer::make-rack-module "Multiplier" #'cl-synthesizer-test::test-module-multiply-by-two)))
      (cl-synthesizer::add-module rack rm1)
      (cl-synthesizer::add-module rack rm2)
      (assert-eq 2 (length (slot-value rack 'cl-synthesizer::modules)))
      (let ((found-module-1 (cl-synthesizer::get-module rack "Counter"))
	    (found-module-2 (cl-synthesizer::get-module rack "Multiplier")))
	(assert-true found-module-1)
	(assert-true found-module-2)
	(assert-equal "Counter" (cl-synthesizer::get-rack-module-name found-module-1))
	(assert-equal "Multiplier" (cl-synthesizer::get-rack-module-name found-module-2))))
    rack))

(define-test test-rack-update-1 ()
	     (let ((rack (create-test-rack-simple)))
	       (let ((counter (cl-synthesizer::get-module rack "Counter"))
		     (multiplier (cl-synthesizer::get-module rack "Multiplier")))
		 (cl-synthesizer::add-patch
		  rack
		  "Counter" :out
		  "Multiplier" :in)
		 (assert-equal 0 (apply (cl-synthesizer::get-rack-module-output-fn counter) (list :out)))
		 (assert-equal 0 (apply (cl-synthesizer::get-rack-module-output-fn multiplier) (list :out)))
		 (cl-synthesizer::update-rack rack)
		 (assert-equal 1 (apply (cl-synthesizer::get-rack-module-output-fn counter) (list :out)))
		 (assert-equal 2 (apply (cl-synthesizer::get-rack-module-output-fn multiplier) (list :out)))
		 (cl-synthesizer::update-rack rack)
		 (assert-equal 2 (apply (cl-synthesizer::get-rack-module-output-fn counter) (list :out)))
		 (assert-equal 4 (apply (cl-synthesizer::get-rack-module-output-fn multiplier) (list :out)))
		 )))


(defun create-test-rack-adder ()
  (let ((rack (make-instance 'cl-synthesizer:rack)))
    (let ((rm1 (cl-synthesizer::make-rack-module "Counter 1" #'cl-synthesizer-test::test-module-counter))
	  (rm2 (cl-synthesizer::make-rack-module "Counter 2" #'cl-synthesizer-test::test-module-counter))
	  (rm3 (cl-synthesizer::make-rack-module "Adder" #'cl-synthesizer-test::test-module-adder)))
      (cl-synthesizer::add-module rack rm1)
      (cl-synthesizer::add-module rack rm2)
      (cl-synthesizer::add-module rack rm3)
      (assert-eq 3 (length (slot-value rack 'cl-synthesizer::modules)))
      (let ((found-module-1 (cl-synthesizer::get-module rack "Counter 1"))
	    (found-module-2 (cl-synthesizer::get-module rack "Counter 2"))
	    (found-module-3 (cl-synthesizer::get-module rack "Adder")))
	(assert-true found-module-1)
	(assert-true found-module-2)
	(assert-true found-module-3)
	(assert-equal "Counter 1" (cl-synthesizer::get-rack-module-name found-module-1))
	(assert-equal "Counter 2" (cl-synthesizer::get-rack-module-name found-module-2))
	(assert-equal "Adder" (cl-synthesizer::get-rack-module-name found-module-3))))
    rack))

(define-test test-rack-update-adder ()
	     (let ((rack (create-test-rack-adder)))
	       (let ((counter-1 (cl-synthesizer::get-module rack "Counter 1"))
		     (counter-2 (cl-synthesizer::get-module rack "Counter 2"))
		     (adder (cl-synthesizer::get-module rack "Adder")))
		 (cl-synthesizer::add-patch
		  rack
		  "Counter 1" :out
		  "Adder" :in-1)
		 (cl-synthesizer::add-patch
		  rack
		  "Counter 2" :out
		  "Adder" :in-2)
		 (assert-equal 0 (apply (cl-synthesizer::get-rack-module-output-fn counter-1) (list :out)))
		 (assert-equal 0 (apply (cl-synthesizer::get-rack-module-output-fn counter-2) (list :out)))
		 (assert-equal 0 (apply (cl-synthesizer::get-rack-module-output-fn adder) (list :out)))
		 (cl-synthesizer::update-rack rack)
		 (assert-equal 1 (apply (cl-synthesizer::get-rack-module-output-fn counter-1) (list :out)))
		 (assert-equal 1 (apply (cl-synthesizer::get-rack-module-output-fn counter-2) (list :out)))
		 (assert-equal 2 (apply (cl-synthesizer::get-rack-module-output-fn adder) (list :out)))
		 (cl-synthesizer::update-rack rack)
		 (assert-equal 2 (apply (cl-synthesizer::get-rack-module-output-fn counter-1) (list :out)))
		 (assert-equal 2 (apply (cl-synthesizer::get-rack-module-output-fn counter-2) (list :out)))
		 (assert-equal 4 (apply (cl-synthesizer::get-rack-module-output-fn adder) (list :out)))
		 )))

(defun create-test-rack-recursive ()
  "Input of adder is connected with one of its outputs"
  (let ((rack (make-instance 'cl-synthesizer:rack)))
    (let ((counter (cl-synthesizer::make-rack-module "Counter" #'cl-synthesizer-test::test-module-counter))
	  (adder (cl-synthesizer::make-rack-module "Adder" #'cl-synthesizer-test::test-module-adder)))
      (cl-synthesizer::add-module rack counter)
      (cl-synthesizer::add-module rack adder)
      (assert-eq 2 (length (slot-value rack 'cl-synthesizer::modules)))
      (let ((found-module-1 (cl-synthesizer::get-module rack "Counter"))
	    (found-module-2 (cl-synthesizer::get-module rack "Adder")))
	(assert-true found-module-1)
	(assert-true found-module-2)
	(assert-equal "Counter" (cl-synthesizer::get-rack-module-name found-module-1))
	(assert-equal "Adder" (cl-synthesizer::get-rack-module-name found-module-2))))
    rack))

(define-test test-rack-update-recursive ()
	     (let ((rack (create-test-rack-recursive)))
	       (let ((counter (cl-synthesizer::get-module rack "Counter"))
		     (adder (cl-synthesizer::get-module rack "Adder")))
		 (cl-synthesizer::add-patch
		  rack
		  "Counter" :out
		  "Adder" :in-1)
		 (cl-synthesizer::add-patch
		  rack
		  "Adder" :out
		  "Adder" :in-2)
		 (assert-equal 0 (apply (cl-synthesizer::get-rack-module-output-fn counter) (list :out)))
		 (assert-equal 0 (apply (cl-synthesizer::get-rack-module-output-fn adder) (list :out)))
		 (cl-synthesizer::update-rack rack)
		 (assert-equal 1 (apply (cl-synthesizer::get-rack-module-output-fn counter) (list :out)))
		 (assert-equal 1 (apply (cl-synthesizer::get-rack-module-output-fn adder) (list :out)))
		 (cl-synthesizer::update-rack rack)
		 (assert-equal 2 (apply (cl-synthesizer::get-rack-module-output-fn counter) (list :out)))
		 (assert-equal 3 (apply (cl-synthesizer::get-rack-module-output-fn adder) (list :out)))
		 (cl-synthesizer::update-rack rack)
		 (assert-equal 3 (apply (cl-synthesizer::get-rack-module-output-fn counter) (list :out)))
		 (assert-equal 6 (apply (cl-synthesizer::get-rack-module-output-fn adder) (list :out)))
		 )))





		 
