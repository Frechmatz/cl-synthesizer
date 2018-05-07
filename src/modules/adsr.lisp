;;
;;
;; ADSR Envelope Generator
;;
;;
;; Work in progress
;;
;;

(in-package :cl-synthesizer-modules-adsr)

(defun adsr (name environment &key (v-peak 5))
  (declare (ignore name))
  (let* ((sample-rate (getf environment :sample-rate))
	 (is-gate nil)
	 (cur-cv 0))
    (declare (ignore sample-rate))
    (list
     :inputs (lambda () '(:gate))
     :outputs (lambda () '(:cv))
     :get-output (lambda (output)
		   (declare (ignore output))
		   cur-cv)
     :update (lambda (&key (gate 0))
	       (setf is-gate (>= gate 4.9))
	       (setf cur-cv (if is-gate v-peak 0))))))

