(in-package :cl-synthesizer-test)

(defun get-module-output (module socket)
  (funcall (getf (funcall (getf module :outputs)) socket)))

(defun update-module (module input-args)
  (let ((sockets nil) (module-inputs (funcall (getf module :inputs))))
    (cl-synthesizer-macro-util:with-property-list input-args socket value
      (push socket sockets)
      (funcall (getf module-inputs socket) value))
    ;; Set missing inputs to nil
    (cl-synthesizer-macro-util:with-property-list module-inputs socket value
      (declare (ignore value))
      (if (not (find socket sockets))
	  (funcall (getf module-inputs socket) nil))))
  (funcall (getf module :update)))

(defun get-module-input-sockets (module)
  "TODO Inefficient implementation, but for now live with it"
  (let ((sockets nil))
    (cl-synthesizer-macro-util:with-property-list (funcall (getf module :inputs)) socket fn
      (declare (ignore fn))
      (push socket sockets))
    sockets))
  
(defun get-module-output-sockets (module)
  "TODO Inefficient implementation, but for now live with it"
  (let ((sockets nil))
    (cl-synthesizer-macro-util:with-property-list (funcall (getf module :outputs)) socket fn
      (declare (ignore fn))
      (push socket sockets))
    sockets))
  


