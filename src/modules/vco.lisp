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
	  (cl-synthesizer-core:sine-core :f-min f-min :f-max f-max :sample-rate sample-rate))
	 (triangle-vco
	  (cl-synthesizer-core:triangle-core :f-min f-min :f-max f-max :sample-rate sample-rate))
	 (saw-vco
	  (cl-synthesizer-core:saw-core :f-min f-min :f-max f-max :sample-rate sample-rate))
	 (square-vco
	  (cl-synthesizer-core:square-core :f-min f-min :f-max f-max :sample-rate sample-rate))
	 (inputs (list :cv))
	 (outputs (list :sine :triangle :saw :square))
	 (cur-sine-output 1.0)
	 (cur-triangle-output 1.0)
	 (cur-saw-output 1.0)
	 (cur-square-output 1.0)
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
		       ((eq output :saw) cur-saw-output)
		       ((eq output :square) cur-square-output)
		       (t (error (format nil "Unknown input ~a requested from VCO" output)))))
       :update (lambda (&key (cv 0))
		 (let ((f (get-frequency cv)))
		   (setf cur-sine-output (* v-peak (funcall (getf sine-vco :tick) f)))
		   (setf cur-triangle-output (* v-peak (funcall (getf triangle-vco :tick) f)))
		   (setf cur-saw-output (* v-peak (funcall (getf saw-vco :tick) f)))
		   (setf cur-square-output (* v-peak (funcall (getf square-vco :tick) f)))
		   ))))))

