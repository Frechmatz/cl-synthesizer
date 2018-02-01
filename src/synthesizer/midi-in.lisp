;;
;; The Midi-IN connector of a rack.
;;

(in-package :cl-synthesizer)

(defun midi-in (name environment)
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
     :outputs (lambda () '(:midi-event))
     :get-output (lambda (output)
		   (declare (ignore output))
		   (if device
		       (let ((f (getf device :get-output)))
			 (if f (funcall f :midi-event) nil))
		       nil))
     :update (lambda ()
	       nil))))

