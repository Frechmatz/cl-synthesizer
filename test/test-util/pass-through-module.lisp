(in-package :cl-synthesizer-test)

(defun pass-through-module (name environment)
  "Module that passes through inputs (:input-1 :input-2) to outputs (:output-1 :output-2)"
  (declare (ignore environment))
  (let ((shutdown-called nil) (output-1 0) (output-2 0) (output-3 0) (input-1 nil) (input-2 nil) (input-3 nil))
    (list
     :state (lambda(state)
		  (cond
		    ((eq state :module-name)
		     name)
		    ((eq state :shutdown-called)
		     shutdown-called)
		    (t
		      (error (format nil "Unknown state '~a' requested frm module pass-through-module" state)))))
     :inputs (lambda ()
	       (list
		:input-1 (list
			  :set (lambda(value) (setf input-1 value))
			  :get (lambda() input-1))
		:input-2 (list
			  :set (lambda(value) (setf input-2 value))
			  :get (lambda() input-2))
		:input-3 (list
			  :set (lambda(value) (setf input-3 value))
			  :get (lambda() input-3))))
     :outputs (lambda ()
		(list
		 :output-1 (list :get (lambda() output-1))
		 :output-2 (list :get (lambda() output-2))
		 :output-3 (list :get (lambda() output-3))))
     :update (lambda ()
	       (setf output-1 input-1)
	       (setf output-2 input-2)
	       (setf output-3 input-3))
     :shutdown (lambda() (setf shutdown-called t)))))


