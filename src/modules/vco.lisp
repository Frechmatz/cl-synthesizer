;;
;;
;; A high-level VCO module supporting multiple wave forms, etc.
;;
;; Work in progress
;;
;;
(in-package :cl-synthesizer-modules-vco)

#|

(defun vco-exponential (name environment &key (base-frequency 440) (f-max 12000) (v-peak 5))
  (if (> 0.0 base-frequency)
      (cl-synthesizer:signal-assembly-error
       :format-control "Base frequency of VCO ~a must be greater than 0: ~a"
       :format-arguments (list name base-frequency)))
  (if (> 0.0 f-max)
      (cl-synthesizer:signal-assembly-error
       :format-control "Max frequency of VCO ~a must be greater than 0: ~a"
       :format-arguments (list name f-max)))
  (let* ((sample-rate (getf environment :sample-rate))
	 (transfer-function-exp
	  (lambda (input-value)
	    (* base-frequency (expt 2 input-value))))
	 (phase-generator (cl-synthesizer-core:phase-generator sample-rate))
	 (cur-sine-output 1.0)
	 (cur-triangle-output 1.0)
	 (cur-saw-output 1.0)
	 (cur-square-output 1.0))
    (flet ((get-frequency (cv)
	     (let ((f (funcall transfer-function-exp cv)))
	       (if (> f f-max)
		   (setf f f-max))
	       (if (< f 0.0)
		   (setf f 0.0))
	       f)))
      (list
       :inputs (lambda () '(:cv))
       :outputs (lambda () '(:sine :triangle :saw :square))
       :get-output (lambda (output)
		     (cond
		       ((eq output :sine) cur-sine-output)
		       ((eq output :triangle) cur-triangle-output)
		       ((eq output :saw) cur-saw-output)
		       ((eq output :square) cur-square-output)
		       (t (error (format nil "Unknown input ~a requested from ~a" output name)))))
       :update (lambda (&key cv)
		 (if (not cv)
		     (setf cv 0))
		 (let* ((f (get-frequency cv))
			(phi (funcall phase-generator f)))
		   (setf cur-sine-output (* v-peak (cl-synthesizer-core:phase-sine-converter phi)))
		   (setf cur-triangle-output (* v-peak (cl-synthesizer-core:phase-triangle-converter phi)))
		   (setf cur-saw-output (* v-peak (cl-synthesizer-core:phase-saw-converter phi)))
		   (setf cur-square-output (* v-peak (cl-synthesizer-core:phase-square-converter phi)))
		   ))))))



(defun vco-linear (name environment &key (cv-max 5) (f-max 12000) (v-peak 5))
  (if (> 0.0 f-max)
      (cl-synthesizer:signal-assembly-error
       :format-control "Max frequency of VCO ~a must be greater than 0: ~a"
       :format-arguments (list name f-max)))
  (let* ((sample-rate (getf environment :sample-rate))
	 (transfer-function-lin
	  (getf (cl-synthesizer-core:linear-converter
		 :input-min 0.0
		 :input-max cv-max
		 :output-min 0.0
		 :output-max f-max)
		:get-y))
	 (phase-generator (cl-synthesizer-core:phase-generator sample-rate))
	 (cur-sine-output 1.0)
	 (cur-triangle-output 1.0)
	 (cur-saw-output 1.0)
	 (cur-square-output 1.0))
    (flet ((get-frequency (cv)
	     (let ((f (funcall transfer-function-lin cv)))
	       (if (> f f-max)
		   (setf f f-max))
	       (if (< f 0.0)
		   (setf f 0.0))
	       f)))
      (list
       :inputs (lambda () '(:cv))
       :outputs (lambda () '(:sine :triangle :saw :square))
       :get-output (lambda (output)
		     (cond
		       ((eq output :sine) cur-sine-output)
		       ((eq output :triangle) cur-triangle-output)
		       ((eq output :saw) cur-saw-output)
		       ((eq output :square) cur-square-output)
		       (t (error (format nil "Unknown input ~a requested from ~a" output name)))))
       :update (lambda (&key cv)
		 (if (not cv)
		     (setf cv 0))
		 (let* ((f (get-frequency cv))
			(phi (funcall phase-generator f)))
		   (setf cur-sine-output (* v-peak (cl-synthesizer-core:phase-sine-converter phi)))
		   (setf cur-triangle-output (* v-peak (cl-synthesizer-core:phase-triangle-converter phi)))
		   (setf cur-saw-output (* v-peak (cl-synthesizer-core:phase-saw-converter phi)))
		   (setf cur-square-output (* v-peak (cl-synthesizer-core:phase-square-converter phi)))
		   ))))))

|#


;; (defun vco-ng (name environment &key (mode :linear) (base-frequency 440) (f-max 12000) (cv-max nil) (v-peak 5))
(defun vco-ng (name environment &key mode base-frequency f-max cv-max v-peak)
  "cv-max Control voltage which represents the maximum output frequency"
  (if (not base-frequency)
      (cl-synthesizer:signal-assembly-error
       :format-control "Base frequency of VCO ~a must not be nil"
       :format-arguments (list name)))
  (if (not f-max)
      (cl-synthesizer:signal-assembly-error
       :format-control "Max frequency of VCO ~a must not be nil"
       :format-arguments (list name)))
  (if (not v-peak)
      (cl-synthesizer:signal-assembly-error
       :format-control "v-peak of VCO ~a must not be nil"
       :format-arguments (list name)))
  (if (> 0.0 base-frequency)
      (cl-synthesizer:signal-assembly-error
       :format-control "Base frequency of VCO ~a must be greater than 0: ~a"
       :format-arguments (list name base-frequency)))
  (if (> 0.0 f-max)
      (cl-synthesizer:signal-assembly-error
       :format-control "Max frequency of VCO ~a must be greater than 0: ~a"
       :format-arguments (list name f-max)))
  (let* ((sample-rate (getf environment :sample-rate))
	 (phase-generator (cl-synthesizer-core:phase-generator sample-rate))
	 (cur-sine-output 1.0)
	 (cur-triangle-output 1.0)
	 (cur-saw-output 1.0)
	 (cur-square-output 1.0)
	 (transfer-function nil))
    (cond
      ((eq mode :exponential)
       (setf transfer-function
	     (lambda (cv)
	       (* base-frequency (expt 2 cv)))))
      ((eq mode :linear)
       (if (not cv-max)
	   (cl-synthesizer:signal-assembly-error
	    :format-control "VCO ~a: In linear mode :cv-max must not be nil"
	    :format-arguments (list name)))
       (let* ((linear-converter
	       (cl-synthesizer-core:linear-converter
		:input-min 0.0
		:input-max cv-max
		:output-min 0.0
		:output-max f-max))
	      (cv-gain (funcall (getf linear-converter :get-x) base-frequency)))
	 (setf transfer-function
	       (lambda (cv)
		 (let ((cur-cv (+ cv cv-gain)))
		   ;; apply clipping
		   (if (> 0.0 cur-cv)
		       (setf cur-cv 0.0))
		   (funcall (getf linear-converter :get-y) cur-cv))))))
      (t
       (cl-synthesizer:signal-assembly-error
	:format-control "Mode of VCO ~a must be one of :linear, :exponential :~a"
	:format-arguments (list name mode))))
    (flet ((get-frequency (cv)
	     (let ((f (funcall transfer-function cv)))
	       (if (> f f-max)
		   (setf f f-max))
	       (if (< f 0.0)
		   (setf f 0.0))
	       f)))
      (list
       :inputs (lambda () '(:cv))
       :outputs (lambda () '(:sine :triangle :saw :square))
       :get-output (lambda (output)
		     (cond
		       ((eq output :sine) cur-sine-output)
		       ((eq output :triangle) cur-triangle-output)
		       ((eq output :saw) cur-saw-output)
		       ((eq output :square) cur-square-output)
		       (t (error (format nil "Unknown input ~a requested from ~a" output name)))))
       :update (lambda (&key cv)
		 (if (not cv)
		     (setf cv 0))
		 (let* ((f (get-frequency cv))
			(phi (funcall phase-generator f)))
		   (setf cur-sine-output (* v-peak (cl-synthesizer-core:phase-sine-converter phi)))
		   (setf cur-triangle-output (* v-peak (cl-synthesizer-core:phase-triangle-converter phi)))
		   (setf cur-saw-output (* v-peak (cl-synthesizer-core:phase-saw-converter phi)))
		   (setf cur-square-output (* v-peak (cl-synthesizer-core:phase-square-converter phi)))
		   ))))))

;; (defun vco-exponential (name environment &key (base-frequency 440) (f-max 12000) (v-peak 5))
(defun vco-exponential (name environment &key base-frequency f-max v-peak)
  (vco-ng name environment :mode :exponential :base-frequency base-frequency :f-max f-max :v-peak v-peak))

;; (defun vco-linear (name environment &key (base-frequency 440) (f-max 12000) (v-peak 5) (cv-max nil))
(defun vco-linear (name environment &key base-frequency f-max v-peak cv-max)
  (vco-ng name environment :mode :linear :base-frequency base-frequency :f-max f-max :v-peak v-peak :cv-max cv-max))

