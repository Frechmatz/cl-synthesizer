
(in-package :cl-synthesizer-test)

(defun create-test-rack ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    (cl-synthesizer:add-module rack "Module 1" #'cl-synthesizer-test::pass-through-module)
    (cl-synthesizer:add-module rack "Module 2" #'cl-synthesizer-test::pass-through-module)
    ;; plus 2 default modules of the rack
    (assert-eq 4 (length (cl-synthesizer:get-modules rack)))
    (let ((found-module-1 (cl-synthesizer:get-module rack "Module 1"))
	  (found-module-2 (cl-synthesizer:get-module rack "Module 2")))
      (assert-true found-module-1)
      (assert-true found-module-2)
      (assert-equal "Module 1" (cl-synthesizer:get-module-name rack found-module-1))
      (assert-equal "Module 2" (cl-synthesizer:get-module-name rack  found-module-2)))
    rack))

(define-test test-simple-patch ()
	     (let ((rack (create-test-rack)))
	       (let ((rm-1 (cl-synthesizer:get-module rack "Module 1"))
		     (rm-2 (cl-synthesizer:get-module rack "Module 2")))
		 (cl-synthesizer:add-patch
		  rack
		  "Module 1" :output-1
		  "Module 2" :input-1)
		 (let ((input-patch (cl-synthesizer-test::get-module-input-patch rack rm-2 :input-1)))
		   (assert-true input-patch)
		   (assert-equal "Module 1" (cl-synthesizer:get-patch-output-name input-patch)))
		 (let ((output-patch (cl-synthesizer-test::get-module-output-patch rack rm-1 :output-1)))
		   (assert-true output-patch)
		   (assert-equal "Module 2" (cl-synthesizer:get-patch-input-name output-patch)))

		 (assert-false (cl-synthesizer-test::get-module-input-patch rack rm-2 :input-2))
		 (assert-false (cl-synthesizer-test::get-module-output-patch rack rm-1 :output-2))
		 (assert-false (cl-synthesizer-test::get-module-input-patch rack rm-1 :input-1))
		 (assert-false (cl-synthesizer-test::get-module-input-patch rack rm-1 :input-2)))

	       (multiple-value-bind (module-name module socket)
		   (cl-synthesizer-test::get-patch rack "Module 2" :input-socket :input-1)
		 (assert-equal "Module 1" module-name)
		 (assert-eq (cl-synthesizer:get-module rack "Module 1") module)
		 (assert-equal :output-1 socket))
	       (multiple-value-bind (module-name module socket)
	           (cl-synthesizer-test::get-patch rack "Module 1" :output-socket :output-1)
	         (assert-equal "Module 2" module-name)
	         (assert-eq (cl-synthesizer:get-module rack "Module 2") module)
	         (assert-equal :input-1 socket))
	       (multiple-value-bind (module-name module socket)
	           (cl-synthesizer-test::get-patch rack "Module XX" :input-socket :input-1)
	         (assert-false module-name)
	         (assert-false module)
	         (assert-false socket))
	       (multiple-value-bind (module-name module socket)
		   (cl-synthesizer-test::get-patch rack "Module XX" :output-socket :input-1)
	       	 (assert-false module-name)
	         (assert-false module)
	         (assert-false socket))
	       (expect-invalid-arguments-error
		 (cl-synthesizer-test::get-patch rack "Module 1" :xx-socket :input-1))
	       ))

(define-test test-connect-sockets-twice-patch ()
	     (let ((rack (create-test-rack)))
	       (let ((rm-1 (cl-synthesizer:get-module rack "Module 1"))
		     (rm-2 (cl-synthesizer:get-module rack "Module 2")))
		 (cl-synthesizer:add-patch
		  rack
		  "Module 1" :output-1
		  "Module 2" :input-1)
		 ;; connect output-1 of "Module 1" once more 
		 (expect-assembly-error
		   (cl-synthesizer:add-patch
		    rack
		    "Module 1" :output-1
		    "Module 2" :input-2))
		 ;; connect cv-1 of "Module 2" once more 
		 (expect-assembly-error
		   (cl-synthesizer:add-patch
		    rack
		    "Module 1" :output-2
		    "Module 2" :input-1))
		 (let ((input-patch (cl-synthesizer-test::get-module-input-patch rack rm-2 :input-1)))
		   (assert-true input-patch)
		   (assert-equal "Module 1" (cl-synthesizer:get-patch-output-name input-patch)))
		 (let ((output-patch (cl-synthesizer-test::get-module-output-patch rack rm-1 :output-1)))
		   (assert-true output-patch)
		   (assert-equal "Module 2" (cl-synthesizer:get-patch-input-name output-patch)))
		 (assert-false (cl-synthesizer-test::get-module-input-patch rack rm-2 :input-2))
		 (assert-false (cl-synthesizer-test::get-module-output-patch rack rm-1 :output-2))
		 (assert-false (cl-synthesizer-test::get-module-input-patch rack rm-1 :input-1))
		 (assert-false (cl-synthesizer-test::get-module-input-patch rack rm-1 :input-2)))))

(define-test test-short-circuit ()
	     (let ((rack (create-test-rack)))
	       (expect-assembly-error
		 (cl-synthesizer:add-patch
		  rack
		  "Module 1" :output-1
		  "Module 1" :output-2))
	       (expect-assembly-error
		 (cl-synthesizer:add-patch
		  rack
		  "Module 1" :input-1
		  "Module 1" :input-2))))

(define-test test-unknown-module ()
	     (let ((rack (create-test-rack)))
	       (expect-assembly-error
		 (cl-synthesizer:add-patch
		  rack
		  "Module 1" :output-1
		  "Module 3" :input-1))
	       (expect-assembly-error
		 (cl-synthesizer:add-patch
		  rack
		  "Module 3" :output-1
		  "Module 1" :input-1))))

(define-test test-unknown-socket ()
	     (let ((rack (create-test-rack)))
		 (expect-assembly-error
		   (cl-synthesizer:add-patch
		    rack
		    "Module 1" :output-99
		    "Module 2" :input-1))
		 (expect-assembly-error
		   (cl-synthesizer:add-patch
		    rack
		    "Module 1" :output-1
		    "Module 2" :input-99))))

