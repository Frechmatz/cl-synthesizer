

(in-package :cl-synthesizer)

;;
;;
;; A simple event logger.
;; The logger is not thread safe.
;;
;;

(defun event-logger ()
  (let ((events nil))
    (list
     :get-events (lambda () events)
     :register-event
     (lambda (module-name event-name)
       (let ((event (event module-name event-name)))
	 (push event events)
	 (getf event :tick)))
     :reset (lambda()
	      (dolist (event events)
		(funcall (getf event :reset))))
     )))

(defun event (module-name event-name)
  (let ((count nil))
    (list
     :tick (lambda() (setf count (if count (+ count 1) 1) ))
     :reset (lambda() (setf count nil))
     :count (lambda() count)
     :module-name (lambda() module-name)
     :event-name (lambda() event-name)
     )))

