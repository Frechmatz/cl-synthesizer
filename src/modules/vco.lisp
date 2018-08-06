;;
;;
;; A high-level VCO module supporting multiple wave forms, etc.
;;
;; Work in progress
;;
;;
(in-package :cl-synthesizer-modules-vco)

(defun vco (name environment &key (base-frequency 440) (cv-linear-max 5) (f-max 12000) (v-peak 5))
  (let* ((sample-rate (getf environment :sample-rate))
	 (transfer-function-exp
	  (lambda (input-value)
	    (* base-frequency (expt 2 input-value))))
	 (transfer-function-lin
	  (getf (cl-synthesizer-core:linear-converter
		 :input-min (* -1 cv-linear-max)
		 :input-max cv-linear-max
		 :output-min (* -1 f-max)
		 :output-max f-max)
		:get-y))
	 (phase-generator (cl-synthesizer-core:phase-generator sample-rate))
	 (cur-sine-output 1.0)
	 (cur-triangle-output 1.0)
	 (cur-saw-output 1.0)
	 (cur-square-output 1.0))
    (flet ((get-frequency (cv-exp cv-lin)
	     (let ((f (+ 
		       (funcall transfer-function-exp cv-exp)
		       (funcall transfer-function-lin cv-lin))))
	       (if (> f f-max)
		   (setf f f-max))
	       (if (< f 0.0)
		   (setf f 0.0))
	       f)))
      (list
       :inputs (lambda () '(:cv :cv-linear))
       :outputs (lambda () '(:sine :triangle :saw :square))
       :get-output (lambda (output)
		     (cond
		       ((eq output :sine) cur-sine-output)
		       ((eq output :triangle) cur-triangle-output)
		       ((eq output :saw) cur-saw-output)
		       ((eq output :square) cur-square-output)
		       (t (error (format nil "Unknown input ~a requested from ~a" output name)))))
       :update (lambda (&key cv cv-linear)
		 (if (not cv)
		     (setf cv 0))
		 (if (not cv-linear)
		     (setf cv-linear 0))
		 (let* ((f (get-frequency cv cv-linear))
			(phi (funcall phase-generator f)))
		   (setf cur-sine-output (* v-peak (cl-synthesizer-core:phase-sine-converter phi)))
		   (setf cur-triangle-output (* v-peak (cl-synthesizer-core:phase-triangle-converter phi)))
		   (setf cur-saw-output (* v-peak (cl-synthesizer-core:phase-saw-converter phi)))
		   (setf cur-square-output (* v-peak (cl-synthesizer-core:phase-square-converter phi)))
		   ))))))

