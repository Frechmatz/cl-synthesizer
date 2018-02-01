;;
;;
;; A high-level VCO module supporting multiple wave forms, etc.
;;
;; Work in progress
;;
;;
(in-package :cl-synthesizer-modules-vco)

(defun vco (name environment &key (f-0 440) (cv-min -5) (cv-max 5) (f-min 0) (f-max 12000) (v-peak 5))
  (let* ((sample-rate (getf environment :sample-rate))
	 (transfer-function
	  (cl-synthesizer-core:linear-converter :input-min cv-min :input-max cv-max :output-min f-min :output-max f-max))
	 (phase-generator (cl-synthesizer-core:phase-generator sample-rate))
	 (inputs (list :cv))
	 (outputs (list :sine :triangle :saw :square))
	 (cur-sine-output 1.0)
	 (cur-triangle-output 1.0)
	 (cur-saw-output 1.0)
	 (cur-square-output 1.0)
	 (cv-offs (funcall (getf transfer-function :output-to-input) f-0)))
    (flet ((get-frequency (cv)
	     (funcall (getf transfer-function :input-to-output) (+ cv cv-offs))))
      (list
       :shutdown (lambda () nil)
       :inputs (lambda () inputs)
       :outputs (lambda () outputs)
       :get-output (lambda (output)
		     (cond
		       ((eq output :sine) cur-sine-output)
		       ((eq output :triangle) cur-triangle-output)
		       ((eq output :saw) cur-saw-output)
		       ((eq output :square) cur-square-output)
		       (t (error (format nil "Unknown input ~a requested from ~a" output name)))))
       :update (lambda (&key (cv 0))
		 (let* ((f (get-frequency cv))
			(phi (funcall phase-generator f)))
		   (setf cur-sine-output (* v-peak (cl-synthesizer-core:phase-sine-converter phi)))
		   (setf cur-triangle-output (* v-peak (cl-synthesizer-core:phase-triangle-converter phi)))
		   (setf cur-saw-output (* v-peak (cl-synthesizer-core:phase-saw-converter phi)))
		   (setf cur-square-output (* v-peak (cl-synthesizer-core:phase-square-converter phi)))
		   ))))))

