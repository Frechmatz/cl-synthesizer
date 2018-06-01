;;
;;
;; A high-level VCO module supporting multiple wave forms, etc.
;;
;; Work in progress
;;
;;
(in-package :cl-synthesizer-modules-vco)

;; CV: exponential input
(defun vco (name environment &key (base-frequency 440) (cv-max 5) (f-max 12000) (v-peak 5))
  (let* ((sample-rate (getf environment :sample-rate))
	 (transfer-function-exp
	  (cl-synthesizer-core:exponential-converter :base-value base-frequency))
	 (transfer-function-lin
	  (cl-synthesizer-core:linear-converter
	   ;; resulting frequency is added to frequency of exp converter
	   :input-min (* -1 cv-max)
	   :input-max cv-max
	   :output-min (* -1 f-max)
	   :output-max f-max))
	 (phase-generator (cl-synthesizer-core:phase-generator sample-rate))
	 (cur-sine-output 1.0)
	 (cur-triangle-output 1.0)
	 (cur-saw-output 1.0)
	 (cur-square-output 1.0))
    (flet ((get-frequency (cv-exp cv-lin)
	     (+ 
	      (funcall (getf transfer-function-exp :get-y) cv-exp)
	      (funcall (getf transfer-function-lin :get-y) cv-lin))))
      (list
       :inputs (lambda () '(:cv :cv-lin))
       :outputs (lambda () '(:sine :triangle :saw :square))
       :get-output (lambda (output)
		     (cond
		       ((eq output :sine) cur-sine-output)
		       ((eq output :triangle) cur-triangle-output)
		       ((eq output :saw) cur-saw-output)
		       ((eq output :square) cur-square-output)
		       (t (error (format nil "Unknown input ~a requested from ~a" output name)))))
       :update (lambda (&key (cv 0) (cv-lin 0))
		 (let* ((f (get-frequency cv cv-lin))
			(phi (funcall phase-generator f)))
		   (setf cur-sine-output (* v-peak (cl-synthesizer-core:phase-sine-converter phi)))
		   (setf cur-triangle-output (* v-peak (cl-synthesizer-core:phase-triangle-converter phi)))
		   (setf cur-saw-output (* v-peak (cl-synthesizer-core:phase-saw-converter phi)))
		   (setf cur-square-output (* v-peak (cl-synthesizer-core:phase-square-converter phi)))
		   ))))))

