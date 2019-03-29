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
		 (update-module rack nil)
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


;;
;; Create a monitor-handler function that
;; sits on a pre-instantiated backend implementation.
;; cl-synthesizer-monitor:add-monitor does not expose the
;; instantiated backend but we want to check its status.
;;

(defun test-monitor-make-handler (module)
  (lambda (name environment inputs)
    (declare (ignore name environment))
    (let ((socket-mapping nil))
      (cond
	((= 1 (length inputs))
	 (setf socket-mapping (list :input-1)))
	((= 2 (length inputs))
	 (setf socket-mapping (list :input-1 :input-2)))
	((< 2 (length inputs))
	 (setf socket-mapping (list :input-1 :input-2 :input-3))))
      (values module socket-mapping))))

(define-test test-monitor-3 ()
	     (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
	       (cl-synthesizer:add-module rack "Counter" #'cl-synthesizer-test::update-counter-module)
	       (cl-synthesizer:add-module rack "Multiplier" #'cl-synthesizer-test::multiplier-module)
	       (cl-synthesizer:add-patch
		rack
		"Counter" :out
		"Multiplier" :in)
	       (let ((monitor-backend-module (pass-through-module "Backend" (cl-synthesizer:get-environment rack))))
		 (cl-synthesizer-monitor:add-monitor
		  rack
		  (test-monitor-make-handler monitor-backend-module)
		  '(("Multiplier" :output-socket :out)
		    ("Multiplier" :input-socket :in)
		    ("Counter" :state :module-name)))
		 (update-module rack nil)
		 (assert-equal 2 (get-module-output monitor-backend-module :output-1))
		 (assert-equal 1 (get-module-output monitor-backend-module :output-2))
		 (assert-equal "Counter" (get-module-output monitor-backend-module :output-3))
		 (funcall (getf rack :shutdown))
		 (assert-true (funcall (getf monitor-backend-module :get-state) :shutdown-called)))))

(define-test test-monitor-unpatched-input ()
	     (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
	       (cl-synthesizer:add-module rack "Counter" #'cl-synthesizer-test::update-counter-module)
	       (cl-synthesizer:add-module rack "Multiplier" #'cl-synthesizer-test::multiplier-module)
	       (let ((monitor-backend-module (pass-through-module "Backend" (cl-synthesizer:get-environment rack))))
		 (cl-synthesizer-test::expect-assembly-error
		   (cl-synthesizer-monitor:add-monitor
		    rack
		    (test-monitor-make-handler monitor-backend-module)
		    '(("Multiplier" :output-socket :out)
		      ("Multiplier" :input-socket :in)
		      ("Counter" :state :module-name)))))))

