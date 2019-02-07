(in-package :cl-synthesizer-test)

(defun create-test-rack-simple ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Counter" #'cl-synthesizer-test::update-counter-module)
    (cl-synthesizer:add-module rack "Multiplier" #'cl-synthesizer-test::multiplier-module)
    ;; plus 2 default modules of the rack
    (assert-eq 4 (length (cl-synthesizer:get-modules rack)))
    (let ((found-module-1 (funcall (getf rack :get-module-by-name) "Counter"))
	  (found-module-2 (funcall (getf rack :get-module-by-name) "Multiplier")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-equal "Counter" (cl-synthesizer:get-module-name rack  found-module-1))
      (assert-equal "Multiplier" (cl-synthesizer:get-module-name rack  found-module-2)))
    rack))

(define-test test-rack-update-1 ()
	     (let ((rack (create-test-rack-simple)))
	       (let ((counter (funcall (getf rack :get-module-by-name) "Counter"))
		     (multiplier (funcall (getf rack :get-module-by-name) "Multiplier")))
		 (cl-synthesizer:add-patch
		  rack
		  "Counter" :out
		  "Multiplier" :in)
		 (assert-equal 0 (funcall (getf counter :get-output) :out))
		 (assert-equal 0 (funcall (getf multiplier :get-output) :out))
		 (funcall (getf rack :update) nil)
		 (assert-equal 1 (funcall (getf counter :get-output) :out))
		 (assert-equal 2 (funcall (getf multiplier :get-output) :out))
		 (funcall (getf rack :update) nil)
		 (assert-equal 2 (funcall (getf counter :get-output) :out))
		 (assert-equal 4 (funcall (getf multiplier :get-output) :out)))))

(defun create-test-rack-adder ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Counter 1" #'cl-synthesizer-test::update-counter-module)
    (cl-synthesizer:add-module rack "Counter 2" #'cl-synthesizer-test::update-counter-module)
    (cl-synthesizer:add-module rack "Adder" #'cl-synthesizer-test::input-adder-module)
    ;; plus 2 default modules of the rack
    (assert-eq 5 (length (cl-synthesizer:get-modules rack)))
    (let ((found-module-1 (funcall (getf rack :get-module-by-name) "Counter 1"))
	  (found-module-2 (funcall (getf rack :get-module-by-name) "Counter 2"))
	  (found-module-3 (funcall (getf rack :get-module-by-name) "Adder")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-true found-module-3)
      (assert-equal "Counter 1" (cl-synthesizer:get-module-name rack  found-module-1))
      (assert-equal "Counter 2" (cl-synthesizer:get-module-name rack  found-module-2))
      (assert-equal "Adder" (cl-synthesizer:get-module-name rack  found-module-3)))
    rack))

(define-test test-rack-update-adder ()
	     (let ((rack (create-test-rack-adder)))
	       (let ((counter-1 (funcall (getf rack :get-module-by-name) "Counter 1"))
		     (counter-2 (funcall (getf rack :get-module-by-name) "Counter 2"))
		     (adder (funcall (getf rack :get-module-by-name) "Adder")))
		 (cl-synthesizer:add-patch
		  rack
		  "Counter 1" :out
		  "Adder" :in-1)
		 (cl-synthesizer:add-patch
		  rack
		  "Counter 2" :out
		  "Adder" :in-2)
		 (assert-equal 0 (apply (getf counter-1 :get-output) (list :out)))
		 (assert-equal 0 (apply (getf counter-2 :get-output) (list :out)))
		 (assert-equal 0 (apply (getf adder :get-output) (list :out)))
		 (funcall (getf rack :update) nil)
		 (assert-equal 1 (apply (getf counter-1 :get-output) (list :out)))
		 (assert-equal 1 (apply (getf counter-2 :get-output) (list :out)))
		 (assert-equal 2 (apply (getf adder :get-output) (list :out)))
		 (funcall (getf rack :update) nil)
		 (assert-equal 2 (apply (getf counter-1 :get-output) (list :out)))
		 (assert-equal 2 (apply (getf counter-2 :get-output) (list :out)))
		 (assert-equal 4 (apply (getf adder :get-output) (list :out))))))

(defun create-test-rack-recursive ()
  "Input of adder is connected with one of its outputs"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Counter" #'cl-synthesizer-test::update-counter-module)
    (cl-synthesizer:add-module rack "Adder" #'cl-synthesizer-test::input-adder-module)
    ;; plus 2 default modules of the rack
    (assert-eq 4 (length (cl-synthesizer:get-modules rack)))
    (let ((found-module-1 (funcall (getf rack :get-module-by-name) "Counter"))
	  (found-module-2 (funcall (getf rack :get-module-by-name) "Adder")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-equal "Counter" (cl-synthesizer:get-module-name rack  found-module-1))
      (assert-equal "Adder" (cl-synthesizer:get-module-name rack found-module-2)))
    rack))

(define-test test-rack-update-recursive ()
	     (let ((rack (create-test-rack-recursive)))
	       (let ((counter (funcall (getf rack :get-module-by-name) "Counter"))
		     (adder (funcall (getf rack :get-module-by-name) "Adder")))
		 (cl-synthesizer:add-patch
		  rack
		  "Counter" :out
		  "Adder" :in-1)
		 (cl-synthesizer:add-patch
		  rack
		  "Adder" :out
		  "Adder" :in-2)
		 (assert-equal 0 (apply (getf counter :get-output) (list :out)))
		 (assert-equal 0 (apply (getf adder :get-output) (list :out)))
		 (funcall (getf rack :update) nil)
		 (assert-equal 1 (apply (getf counter :get-output) (list :out)))
		 (assert-equal 1 (apply (getf adder :get-output) (list :out)))
		 (funcall (getf rack :update) nil)
		 (assert-equal 2 (apply (getf counter :get-output) (list :out)))
		 (assert-equal 3 (apply (getf adder :get-output) (list :out)))
		 (funcall (getf rack :update) nil)
		 (assert-equal 3 (apply (getf counter :get-output) (list :out)))
		 (assert-equal 6 (apply (getf adder :get-output) (list :out))))))
