
(in-package :cl-synthesizer-test)

(defun create-test-rack ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Module 1" #'cl-synthesizer-test::pass-through-module)
    (cl-synthesizer:add-module rack "Module 2" #'cl-synthesizer-test::pass-through-module)
    ;; plus 2 default modules of the rack
    (assert-eq 4 (length (funcall (getf rack :rack-modules))))
    (let ((found-module-1 (funcall (getf rack :get-module-by-name) "Module 1"))
	  (found-module-2 (funcall (getf rack :get-module-by-name) "Module 2")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-equal "Module 1" (funcall (getf rack :get-module-name) found-module-1))
      (assert-equal "Module 2" (funcall (getf rack :get-module-name) found-module-2)))
    rack))

(define-test test-simple-patch ()
	     (let ((rack (create-test-rack)))
	       (let ((rm-1 (funcall (getf rack :get-module-by-name) "Module 1"))
		     (rm-2 (funcall (getf rack :get-module-by-name) "Module 2")))
		 (cl-synthesizer:add-patch
		  rack
		  "Module 1" :out-1
		  "Module 2" :cv-1)
		 (let ((input-patch (funcall (getf rack :get-module-input-patch) rm-2 :cv-1)))
		   (assert-true input-patch)
		   (assert-equal "Module 1" (getf input-patch :output-name)))
		 (let ((output-patch (funcall (getf rack :get-module-output-patch) rm-1 :out-1)))
		   (assert-true output-patch)
		   (assert-equal "Module 2" (getf output-patch :input-name)))

		 (assert-false (funcall (getf rack :get-module-input-patch) rm-2 :cv-2))
		 (assert-false (funcall (getf rack :get-module-output-patch) rm-1 :out-2))
		 (assert-false (funcall (getf rack :get-module-input-patch) rm-1 :cv-1))
		 (assert-false (funcall (getf rack :get-module-input-patch) rm-1 :cv-2)))

	       (multiple-value-bind (module-name module socket)
		   (cl-synthesizer:get-patch rack "Module 2" :input-socket :cv-1)
		 (assert-equal "Module 1" module-name)
		 (assert-eq (cl-synthesizer:get-module rack "Module 1") module)
		 (assert-equal :out-1 socket))
	       (multiple-value-bind (module-name module socket)
		   (cl-synthesizer:get-patch rack "Module 1" :output-socket :out-1)
		 (assert-equal "Module 2" module-name)
		 (assert-eq (cl-synthesizer:get-module rack "Module 2") module)
		 (assert-equal :cv-1 socket))
	       (multiple-value-bind (module-name module socket)
		   (cl-synthesizer:get-patch rack "Module XX" :input-socket :cv-1)
		 (assert-false module-name)
		 (assert-false module)
		 (assert-false socket))
	       (multiple-value-bind (module-name module socket)
		   (cl-synthesizer:get-patch rack "Module XX" :output-socket :cv-1)
		 (assert-false module-name)
		 (assert-false module)
		 (assert-false socket))
	       (expect-invalid-arguments-error
		 (cl-synthesizer:get-patch rack "Module 1" :xx-socket :cv-1))
	       ))

(define-test test-connect-sockets-twice-patch ()
	     (let ((rack (create-test-rack)))
	       (let ((rm-1 (funcall (getf rack :get-module-by-name) "Module 1"))
		     (rm-2 (funcall (getf rack :get-module-by-name) "Module 2")))
		 (cl-synthesizer:add-patch
		  rack
		  "Module 1" :out-1
		  "Module 2" :cv-1)
		 ;; connect out-1 of "Module 1" once more 
		 (expect-assembly-error
		   (cl-synthesizer:add-patch
		    rack
		    "Module 1" :out-1
		    "Module 2" :cv-2))
		 ;; connect cv-1 of "Module 2" once more 
		 (expect-assembly-error
		   (cl-synthesizer:add-patch
		    rack
		    "Module 1" :out-2
		    "Module 2" :cv-1))
		 (let ((input-patch (funcall (getf rack :get-module-input-patch) rm-2 :cv-1)))
		   (assert-true input-patch)
		   (assert-equal "Module 1" (getf input-patch :output-name)))
		 (let ((output-patch (funcall (getf rack :get-module-output-patch) rm-1 :out-1)))
		   (assert-true output-patch)
		   (assert-equal "Module 2" (getf output-patch :input-name)))
		 (assert-false (funcall (getf rack :get-module-input-patch) rm-2 :cv-2))
		 (assert-false (funcall (getf rack :get-module-output-patch) rm-1 :out-2))
		 (assert-false (funcall (getf rack :get-module-input-patch) rm-1 :cv-1))
		 (assert-false (funcall (getf rack :get-module-input-patch) rm-1 :cv-2)))))

(define-test test-short-circuit ()
	     (let ((rack (create-test-rack)))
	       (expect-assembly-error
		 (cl-synthesizer:add-patch
		  rack
		  "Module 1" :out-1
		  "Module 1" :out-2))
	       (expect-assembly-error
		 (cl-synthesizer:add-patch
		  rack
		  "Module 1" :cv-1
		  "Module 1" :cv-2))))

(define-test test-unknown-module ()
	     (let ((rack (create-test-rack)))
	       (expect-assembly-error
		 (cl-synthesizer:add-patch
		  rack
		  "Module 1" :out-1
		  "Module 3" :cv-1))
	       (expect-assembly-error
		 (cl-synthesizer:add-patch
		  rack
		  "Module 3" :out-1
		  "Module 1" :cv-1))))

(define-test test-unknown-socket ()
	     (let ((rack (create-test-rack)))
		 (expect-assembly-error
		   (cl-synthesizer:add-patch
		    rack
		    "Module 1" :out-3
		    "Module 2" :cv-1))
		 (expect-assembly-error
		   (cl-synthesizer:add-patch
		    rack
		    "Module 1" :out-1
		    "Module 2" :cv-3))))

