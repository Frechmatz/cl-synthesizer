(in-package :cl-synthesizer-test)

(defun get-module-output (module socket)
  (funcall (getf (getf (funcall (getf module :outputs)) socket) :get)))

(defun update-module (module input-args)
  "input-args: List of (key value)"
  (let ((module-inputs (funcall (getf module :inputs))))
    (dolist (input input-args)
      (let ((socket (first input))
	    (socket-value (second input)))
	(let ((setter (getf (getf module-inputs socket) :set)))
	  (funcall setter socket-value)))))
  (cl-synthesizer:update module))

(defun get-module-input-sockets (module)
  (let ((sockets nil) (counter 0))
    (dolist (input (funcall (getf module :inputs)))
      ;; Property list. Sockets are at indices 0, 2, 4, 6, ...
      (if (= (rem counter 2) 0)
	  (push input sockets))
      (setf counter (+ counter 1)))
    sockets))

(defun get-module-output-sockets (module)
  (let ((sockets nil) (counter 0))
    (dolist (input (funcall (getf module :outputs)))
      ;; Property list. Sockets are at indices 0, 2, 4, 6, ...
      (if (= (rem counter 2) 0)
	  (push input sockets))
      (setf counter (+ counter 1)))
    sockets))

(defun get-module-state (module key)
  (let ((fn (getf module :state)))
    (if fn
	(funcall fn key)
	nil)))

(defun is-module-input (module socket)
  (let ((inputs (funcall (getf module :inputs))))
    (getf inputs socket)))

(defun is-module-output (module socket)
  (let ((outputs (funcall (getf module :outputs))))
    (getf outputs socket)))

