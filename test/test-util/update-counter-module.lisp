(in-package :cl-synthesizer-test)

(defun update-counter-module (name environment)
  "Module with no inputs that counts invocations of the update function and exposes the count via output :out"
  (declare (ignore environment))
  (let ((out 0) (shutdown-called nil))
    (list
     :get-state (lambda(state)
		  (cond 
		    ((eq state :module-name)
		     name)
		    ((eq state :shutdown-called)
		     shutdown-called)
		    (t 
		      (error (format nil "Unknown state ~a requested frm module update-counter-module" state)))))
     :inputs (lambda () nil)
     :outputs (lambda () (list :out))
     :get-output (lambda (output)
		     (cond 
		       ((eq :out output)
			out)
		       (t (error (format nil "Unknown output ~a requested from update-counter-module" output)))))
     :update (lambda (input-args)
	       (declare (ignore input-args))
	       (setf out (+ 1 out)))
     :shutdown (lambda()
		 (setf shutdown-called t)))))
