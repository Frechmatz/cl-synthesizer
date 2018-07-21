;;
;; The Midi-IN connector of a rack.
;;

(in-package :cl-synthesizer)

(defun midi-in-adapter (name environment)
  (declare (ignore name))
  (declare (ignore environment))
  (let ((device nil))
    (list
     :set-device (lambda (d) (setf device d))
     :shutdown (lambda ()
		 (if device
		     (let ((f (getf device :shutdown)))
		       (if f (funcall f)))))
     :inputs (lambda () nil)
     :outputs (lambda () '(:midi-events))
     :get-output (lambda (output)
		   (declare (ignore output))
		   (if device
		       (let ((f (getf device :get-output)))
			 (if f (funcall f :midi-events) nil))
		       nil))
     :update (lambda ()
	       nil))))

