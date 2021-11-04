(in-package :cl-synthesizer)

(defun get-inputs (module)
  (getf module :inputs))

(defun get-outputs (module)
  (getf module :outputs))

(defun get-update-fn (module)
  (getf module :update))

(defun get-state-fn (module)
  (getf module :state))

(defun get-shutdown-fn (module)
  (getf module :shutdown))
