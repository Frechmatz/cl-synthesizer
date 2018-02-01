
(in-package :cl-synthesizer-event-logger)

(defmacro with-aggregated-events (event-aggregator event-id count &body body)
  (let ((event (gensym)))
    `(dolist (,event (funcall (getf ,event-aggregator :get-events)))
       (if (second ,event)
	   (let ((,event-id (first ,event))
		 (,count (second ,event)))
	     ,@body)))))

(defun event-aggregator ()
  ;; events: ( (eventId, count) ...)
  (let ((events nil))
    (list
     :add-event
     (lambda (event-id)
       (let ((l (find-if (lambda (i) (eq event-id (first i))) events)))
	 (if (not l)
	     (progn
	       (setf l (list event-id 0))
	       (push l events)))
	 (let ((c (second l)))
	   (setf (second l) (if c (+ c 1) 1)))))
     :reset (lambda()
	      (dolist (e events)
		(setf (second e) nil)))
     :get-events (lambda () events))))
