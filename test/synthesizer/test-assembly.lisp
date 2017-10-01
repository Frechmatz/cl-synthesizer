
(in-package :cl-synthesizer-test)

(define-test test-add-module-to-rack-1 ()
  (let ((rack (make-instance 'cl-synthesizer:rack)))
    (let ((rm (cl-synthesizer::make-rack-module "Module 1" #'cl-synthesizer-test::test-module)))
      (cl-synthesizer::add-module rack rm)
      (assert-eq 1 (length (slot-value rack 'cl-synthesizer::modules)))
      (assert-true (cl-synthesizer::get-module rack "Module 1")))))

(define-test test-add-module-to-rack-2 ()
  (let ((rack (make-instance 'cl-synthesizer:rack)))
    (let ((rm1 (cl-synthesizer::make-rack-module "Module 1" #'cl-synthesizer-test::test-module))
	  (rm2 (cl-synthesizer::make-rack-module "Module 2" #'cl-synthesizer-test::test-module)))
      (cl-synthesizer::add-module rack rm1)
      (cl-synthesizer::add-module rack rm2)
      (assert-eq 2 (length (slot-value rack 'cl-synthesizer::modules)))
      (let ((found-module-1 (cl-synthesizer::get-module rack "Module 1"))
	    (found-module-2 (cl-synthesizer::get-module rack "Module 2")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-equal "Module 1" (cl-synthesizer::get-rack-module-name found-module-1))
      (assert-equal "Module 2" (cl-synthesizer::get-rack-module-name found-module-2))
      ))))

(define-test test-add-module-to-rack-already-exists ()
  (let ((rack (make-instance 'cl-synthesizer:rack)))
    (let ((rm1 (cl-synthesizer::make-rack-module "Module 1" #'cl-synthesizer-test::test-module))
	  (rm2 (cl-synthesizer::make-rack-module "Module 1" #'cl-synthesizer-test::test-module)))
      (cl-synthesizer::add-module rack rm1)
      (expect-assembly-exception
       (cl-synthesizer::add-module rack rm2))
      (assert-eq 1 (length (slot-value rack 'cl-synthesizer::modules)))
      (let ((found-module-1 (cl-synthesizer::get-module rack "Module 1"))
	    (found-module-2 (cl-synthesizer::get-module rack "Module 2")))
      (assert-true found-module-1)
      (assert-false found-module-2)
      (assert-equal "Module 1" (cl-synthesizer::get-rack-module-name found-module-1))
      ))))


#|
(define-test pool-stopping-worker-thread ()
  "Test that pool cannot be stopped by a worker thread"
  (let ((pool (cl-threadpool:make-threadpool 5)))
    (cl-threadpool:start pool)
    (let ((got-error nil))
      (cl-threadpool:add-job
       pool
       (lambda ()
         (handler-case
             (cl-threadpool:stop pool)
           (error (err) (setf got-error err)))))
      (cl-threadpool:stop pool)
      (assert-true got-error)
      (assert-true (typep got-error 'cl-threadpool:threadpool-error)))))
|#
