(in-package :cl-synthesizer-test)

(define-test test-monitor-1 ()
	     (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
	       (cl-synthesizer:add-module rack "Counter" #'cl-synthesizer-test::update-counter-module)
	       (cl-synthesizer:add-module rack "Multiplier" #'cl-synthesizer-test::multiplier-module)
	       (cl-synthesizer:add-patch
		rack
		"Counter" :out
		"Multiplier" :in)
	       (let ((handler-out nil) (handler-in nil) (handler-module-name nil) (shutdown-called nil))
		 ;; Monitor-Handler instantiation function
		 (flet ((instantiate-handler (name environment inputs)
			  (values 
			   (list
			    :update (lambda (input-args)
				      (format t "~%Handler called with ~a~%" input-args)
				      (setf handler-out (getf input-args :out))
				      (setf handler-in (getf input-args :in))
				      (setf handler-module-name (getf input-args :module-name)))
			    :shutdown (lambda() (setf shutdown-called t)))
			   '(:out :in :module-name))))
		   (cl-synthesizer-monitor:add-monitor
		    rack
		    #'instantiate-handler
		    '(("Multiplier" :output-socket :out)
		      ("Multiplier" :input-socket :in)
		      ("Counter" :state :module-name))))
		 (funcall (getf rack :update) nil)
		 (assert-equal "Counter" handler-module-name)
		 (assert-equal 1 handler-in)
		 (assert-equal 2 handler-out)
		 (funcall (getf rack :shutdown))
		 (assert-true shutdown-called)
		 )))

(define-test test-monitor-2 ()
	     (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
	       (flet ((instantiate-handler (name environment inputs)
			(values 
			 (list
			  :update (lambda (input-args) nil))
			 '(:out :in :module-name))))
		 (cl-synthesizer-test::expect-assembly-error
		   (cl-synthesizer-monitor:add-monitor
		    rack
		    #'instantiate-handler
		     '(("Multiplier" :output-socket :out)
		       ("Multiplier" :input-socket :in)
		       ("Counter" :state :module-name)))))))



		  

		  
