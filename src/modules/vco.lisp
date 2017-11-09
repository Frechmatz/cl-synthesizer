;;
;;
;; A high-level VCO module supporting multiple wave forms, etc.
;;
;; Work in progress
;;
;;
(in-package :cl-synthesizer-modules-vco)

(defun vco (environment &key (f-0 440) (cv-min -5) (cv-max 5) (f-min 0) (f-max 12000) (v-peak 5))
  (let* ((sample-rate (getf environment :sample-rate))
	 (transfer-function
	  (cl-synthesizer-core:linear-converter :cv-min cv-min :cv-max cv-max :f-min f-min :f-max f-max))
	 (sine-vco
	  (cl-synthesizer-core:sine-core :f-min f-min :f-max f-max :v-peak v-peak :sample-rate sample-rate))
	 (triangle-vco
	  (cl-synthesizer-core:triangle-core :f-min f-min :f-max f-max :v-peak v-peak :sample-rate sample-rate))
	 (inputs (list :cv))
	 (outputs (list :sine :triangle))
	 (cur-sine-output 1.0)
	 (cur-triangle-output 1.0)
	 (cv-offs (funcall (getf transfer-function :get-cv) f-0)))
    (flet ((get-frequency (cv)
	     (funcall (getf transfer-function :get-frequency) (+ cv cv-offs))))
      (list
       :shutdown (lambda () nil)
       :inputs (lambda () inputs)
       :outputs (lambda () outputs)
       :get-output (lambda (output)
		     (cond
		       ((eq output :sine) cur-sine-output)
		       ((eq output :triangle) cur-triangle-output)
		       (t (error (format nil "Unknown input ~a requested from VCO" output)))))
       :update (lambda (&key (cv 0))
		 (let ((f (get-frequency cv)))
		   (setf cur-sine-output (funcall (getf sine-vco :tick) f))
		   (setf cur-triangle-output (funcall (getf triangle-vco :tick) f))
		   ))))))

