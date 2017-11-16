;;
;;
;; A component that generates trigger events
;;
;; Work in progress
;;
;;

(in-package :cl-synthesizer-core)

(defun trigger (&key delta)
  ""
  (let ((cur-input nil))
    (list
     :is-trigger (lambda (v)
	     (if (not cur-input)
		 (progn 
		   (setf cur-input v)
		   nil)
		 (let ((cur-delta (abs (- cur-input v))))
		   (let ((triggered (if (>= cur-delta delta) t nil)))
		     (if triggered
			 (setf cur-input v))
		     triggered))))
     :reset (lambda () nil))))
