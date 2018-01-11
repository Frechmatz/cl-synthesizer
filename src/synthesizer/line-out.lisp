(in-package :cl-synthesizer)

(defun line-out (environment)
  (declare (ignore environment))
  (let ((device nil))
    (list
     :set-device (lambda (d) (setf device d))
     :shutdown (lambda ()
		 (if device
		     (let ((f (getf device :shutdown)))
		       (if f (funcall f)))))
     :inputs (lambda () '(:channel-1 :channel-2))
     :outputs (lambda () nil)
     :get-output (lambda (output)
		   (declare (ignore output))
		   nil)
     :update (lambda (&key (channel-1 0) (channel-2 0))
	       (if device
		   (let ((f (getf device :update)))
		     (if f (funcall f :channel-1 channel-1 :channel-2 channel-2))))))))

