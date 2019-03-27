(in-package :cl-synthesizer-test)

(defun pass-through-module (name environment)
  "Module that passes through inputs (:cv-1 :cv-2) to outputs (:out-1 :out-2)"
  (declare (ignore environment))
  (let ((out-1 0) (out-2 0) (cv-1 nil) (cv-2 nil))
    (list
     :v2 t
     :get-state (lambda(state)
		  (if (eq state :module-name)
		      name
		      (error (format nil "Unknown state ~a requested frm module pass-through-module" state))))
     :inputs (lambda ()
	       (list
		:cv-1 (lambda(value) (setf cv-1 value))
		:cv-2 (lambda(value) (setf cv-2 value))))
     :outputs (lambda ()
		(list
		 :out-1 (lambda() out-1)
		 :out-2 (lambda() out-2)))
     :update (lambda ()
	       (setf out-1 cv-1)
	       (setf out-2 cv-2)))))

