;;
;; Rack compiler
;;

(in-package :cl-synthesizer-rack-compiler)


(defun compile-rack (rack)
  "Compile a rack."
  (let ((module-updates
	  (mapcar
	   (lambda (module) (funcall (getf rack :compile-module) module))
	   (funcall (getf rack :get-module-trace))))
	(rack-updated-hooks nil)
	(rack-updating-hooks nil))
    (dolist (hook (funcall (getf rack :hooks)))
      (let ((updated (getf hook :updated))
	    (updating (getf hook :updating)))
	(if updating (push updating rack-updating-hooks))
	(if updated (push updated rack-updated-hooks))))
    (lambda ()
      ;; Call hooks
      (dolist (fn rack-updating-hooks)
	(funcall fn))
      ;; Update modules
      (dolist (fn module-updates)
	(funcall fn))
      ;; Call hooks
      (dolist (fn rack-updated-hooks)
	(funcall fn)))))

