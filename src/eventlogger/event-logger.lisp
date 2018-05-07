
(in-package :cl-synthesizer-event-logger)

(defmacro with-transports (callback-selector handler &body body)
  (let ((transport (gensym)))
    `(dolist (,transport transports)
       (let ((,handler (getf (first ,transport) ,callback-selector)))
	 (if handler
	     (progn ,@body))))))

(defun event-logger ()
  (let ((transports nil))
    (list
     :add-transport 
     (lambda (name transport)
       (if (not name)
	   (error "add-transport: Name is mandatory"))
       (push (list transport name) transports))
     :register-event
     (lambda (component-name event-name)
       (let ((event-id
	      (intern (format nil "~a-~a" (string-upcase component-name) (string-upcase event-name)) "KEYWORD")))
	 (lambda ()
	   (with-transports :log handler
	     (funcall handler event-id)))))
     :tick
     (lambda ()
       (with-transports :tick handler
	 (funcall handler)))
     :shutdown
     (lambda ()
       (with-transports :flush handler
	 (funcall handler)))
     )))


