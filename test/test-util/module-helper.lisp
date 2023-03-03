(in-package :cl-synthesizer-test)

(defun get-module-output (module socket)
  (funcall (getf (funcall (getf module :outputs)) socket)))

(defun update-module (module input-args)
  "input-args: List of (key value)"
  (let ((module-inputs (funcall (getf module :inputs))))
    (dolist (input input-args)
      (funcall
       (getf module-inputs (first input)) (second input))))
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
