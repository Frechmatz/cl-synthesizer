;;
;; The line-out connector of a rack.
;;

(in-package :cl-synthesizer)

(defun line-out-adapter (name environment)
  (declare (ignore name))
  (let ((device nil)
	(inputs (cl-synthesizer-macro-util:make-keyword-list
		 "channel"
		 (getf environment :channel-count))))
    (list
     :set-device (lambda (d) (setf device d))
     :shutdown (lambda ()
		 (if device
		     (let ((f (getf device :shutdown)))
		       (if f (funcall f)))))
     :inputs (lambda () inputs)
     :outputs (lambda () nil)
     :get-output (lambda (output)
		   (declare (ignore output))
		   nil)
     :update (lambda (&rest args) ;; to be called with lambda list (:channel-1 v :channel-2 v ...)
	       (if device
		   (let ((f (getf device :update)))
		     (if f (apply f args))))))))

