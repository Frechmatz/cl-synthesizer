(in-package :cl-synthesizer-test)

(defun create-test-rack-simple ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Counter" #'cl-synthesizer-test::update-counter-module)
    (cl-synthesizer:add-module rack "Multiplier" #'cl-synthesizer-test::multiplier-module)
    ;; plus 2 default modules of the rack
    (assert-eq 4 (length (cl-synthesizer:get-modules rack)))
    (let ((found-module-1 (cl-synthesizer:get-module rack "Counter"))
	  (found-module-2 (cl-synthesizer:get-module rack "Multiplier")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-equal "Counter" (cl-synthesizer:get-module-name rack  found-module-1))
      (assert-equal "Multiplier" (cl-synthesizer:get-module-name rack  found-module-2)))
    rack))

(define-test test-rack-update-1 ()
	     (let ((rack (create-test-rack-simple)))
	       (let ((counter (cl-synthesizer:get-module rack "Counter"))
		     (multiplier (cl-synthesizer:get-module rack "Multiplier")))
		 (cl-synthesizer:add-patch
		  rack
		  "Counter" :out
		  "Multiplier" :in)
		 (assert-equal 0 (get-module-output counter :out))
		 (assert-equal 0 (get-module-output multiplier :out))
		 (update-module rack nil)
		 (assert-equal 1 (get-module-output counter :out))
		 (assert-equal 2 (get-module-output multiplier :out))
		 (update-module rack nil)
		 (assert-equal 2 (get-module-output counter :out))
		 (assert-equal 4 (get-module-output multiplier :out))
		 (cl-synthesizer:shutdown rack)
		 (assert-true (cl-synthesizer:get-state counter :shutdown-called)))))

(defun create-test-rack-adder ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Counter 1" #'cl-synthesizer-test::update-counter-module)
    (cl-synthesizer:add-module rack "Counter 2" #'cl-synthesizer-test::update-counter-module)
    (cl-synthesizer:add-module rack "Adder" #'cl-synthesizer-test::input-adder-module)
    ;; plus 2 default modules of the rack
    (assert-eq 5 (length (cl-synthesizer:get-modules rack)))
    (let ((found-module-1 (cl-synthesizer:get-module rack "Counter 1"))
	  (found-module-2 (cl-synthesizer:get-module rack "Counter 2"))
	  (found-module-3 (cl-synthesizer:get-module rack "Adder")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-true found-module-3)
      (assert-equal "Counter 1" (cl-synthesizer:get-module-name rack  found-module-1))
      (assert-equal "Counter 2" (cl-synthesizer:get-module-name rack  found-module-2))
      (assert-equal "Adder" (cl-synthesizer:get-module-name rack  found-module-3)))
    rack))

(define-test test-rack-update-adder ()
	     (let ((rack (create-test-rack-adder)))
	       (let ((counter-1 (cl-synthesizer:get-module rack "Counter 1"))
		     (counter-2 (cl-synthesizer:get-module rack "Counter 2"))
		     (adder (cl-synthesizer:get-module rack "Adder")))
		 (cl-synthesizer:add-patch
		  rack
		  "Counter 1" :out
		  "Adder" :in-1)
		 (cl-synthesizer:add-patch
		  rack
		  "Counter 2" :out
		  "Adder" :in-2)
		 (assert-equal 0 (get-module-output counter-1 :out))
		 (assert-equal 0 (get-module-output counter-2 :out))
		 (assert-equal 0 (get-module-output adder :out))
		 (update-module rack nil)
		 (assert-equal 1 (get-module-output counter-1 :out))
		 (assert-equal 1 (get-module-output counter-2 :out))
		 (assert-equal 2 (get-module-output adder :out))
		 (update-module rack nil)
		 (assert-equal 2 (get-module-output counter-1 :out))
		 (assert-equal 2 (get-module-output counter-2 :out))
		 (assert-equal 4 (get-module-output adder :out)))))

(defun create-test-rack-recursive ()
  "Input of adder is connected with one of its outputs"
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Counter" #'cl-synthesizer-test::update-counter-module)
    (cl-synthesizer:add-module rack "Adder" #'cl-synthesizer-test::input-adder-module)
    ;; plus 2 default modules of the rack
    (assert-eq 4 (length (cl-synthesizer:get-modules rack)))
    (let ((found-module-1 (cl-synthesizer:get-module rack "Counter"))
	  (found-module-2 (cl-synthesizer:get-module rack "Adder")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-equal "Counter" (cl-synthesizer:get-module-name rack  found-module-1))
      (assert-equal "Adder" (cl-synthesizer:get-module-name rack found-module-2)))
    rack))

(define-test test-rack-update-recursive ()
	     (let ((rack (create-test-rack-recursive)))
	       (let ((counter (cl-synthesizer:get-module rack "Counter"))
		     (adder (cl-synthesizer:get-module rack "Adder")))
		 (cl-synthesizer:add-patch
		  rack
		  "Counter" :out
		  "Adder" :in-1)
		 (cl-synthesizer:add-patch
		  rack
		  "Adder" :out
		  "Adder" :in-2)
		 (assert-equal 0 (get-module-output counter :out))
		 (assert-equal 0 (get-module-output adder :out))
		 (update-module rack nil)
		 (assert-equal 1 (get-module-output counter :out))
		 (assert-equal 1 (get-module-output adder :out))
		 (update-module rack nil)
		 (assert-equal 2 (get-module-output counter :out))
		 (assert-equal 3 (get-module-output adder :out))
		 (update-module rack nil)
		 (assert-equal 3 (get-module-output counter :out))
		 (assert-equal 6 (get-module-output adder :out)))))

(define-test test-rack-update-empty-rack ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (update-module rack nil)
    (assert-true t)))

