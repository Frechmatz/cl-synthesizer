;;
;;
;; ADSR Envelope Generator
;;
;;
;; Work in progress
;;
;;

(in-package :cl-synthesizer-modules-adsr)

(defun adsr (environment &key (v-peak 5))
  (let* ((sample-rate (getf environment :sample-rate))
	 (inputs (list :gate))
	 (outputs (list :cv))
	 (is-gate nil)
	 (cur-cv 0))
    (declare (ignore sample-rate))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () inputs)
     :outputs (lambda () outputs)
     :get-output (lambda (output)
		   (declare (ignore output))
		   cur-cv)
     :update (lambda (&key (gate 0))
	       (setf is-gate (>= gate 4.9))
	       (setf cur-cv (if is-gate v-peak 0))))))

