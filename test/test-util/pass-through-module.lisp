(in-package :cl-synthesizer-test)

(defun pass-through-module (name environment)
  "Module that passes through inputs (:input-1 :input-2) to outputs (:output-1 :output-2)"
  (declare (ignore environment))
  (let ((output-1 0) (output-2 0) (input-1 nil) (input-2 nil))
    (list
     :v2 t
     :get-state (lambda(state)
		  (if (eq state :module-name)
		      name
		      (error (format nil "Unknown state ~a requested frm module pass-through-module" state))))
     :inputs (lambda ()
	       (list
		:input-1 (lambda(value) (setf input-1 value))
		:input-2 (lambda(value) (setf input-2 value))))
     :outputs (lambda ()
		(list
		 :output-1 (lambda() output-1)
		 :output-2 (lambda() output-2)))
     :update (lambda ()
	       (setf output-1 input-1)
	       (setf output-2 input-2)))))

