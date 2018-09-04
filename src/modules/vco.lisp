;;
;;
;; Simple Linear/Exponential VCO-Modules
;;
;;

(in-package :cl-synthesizer-modules-vco)

(defun vco-base (name environment transfer-function &key f-max v-peak)
  ""
  (if (not f-max)
      (cl-synthesizer:signal-assembly-error
       :format-control "f-max of VCO ~a must not be nil"
       :format-arguments (list name)))
  (if (> 0.0 f-max)
      (cl-synthesizer:signal-assembly-error
       :format-control "f-max of VCO ~a must be greater than 0: ~a"
       :format-arguments (list name f-max)))
  (if (not v-peak)
      (cl-synthesizer:signal-assembly-error
       :format-control "v-peak of VCO ~a must not be nil"
       :format-arguments (list name)))
  (if (>= 0.0 v-peak)
      (cl-synthesizer:signal-assembly-error
       :format-control "v-peak of VCO ~a must be greater than 0: ~a"
       :format-arguments (list name v-peak)))
  (let* ((sample-rate (getf environment :sample-rate))
	 (phase-generator (cl-synthesizer-core:phase-generator sample-rate))
	 (cur-sine-output 1.0)
	 (cur-triangle-output 1.0)
	 (cur-saw-output 1.0)
	 (cur-square-output 1.0))
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
		   (setf cur-square-output (* v-peak (cl-synthesizer-core:phase-square-converter phi)))))))))

(defun vco-exponential (name environment &key base-frequency f-max v-peak)
  (if (not base-frequency)
      (cl-synthesizer:signal-assembly-error
       :format-control "base-frequency of VCO ~a must not be nil"
       :format-arguments (list name)))
  (if (> 0.0 base-frequency)
      (cl-synthesizer:signal-assembly-error
       :format-control "base-frequency of VCO ~a must be greater than 0: ~a"
       :format-arguments (list name base-frequency)))
  (vco-base
   name
   environment
   (lambda (cv)
     (* base-frequency (expt 2 cv)))
   :f-max f-max
   :v-peak v-peak))

(defun vco-linear (name environment &key base-frequency f-max v-peak cv-max)
  (if (not base-frequency)
      (cl-synthesizer:signal-assembly-error
       :format-control "base-frequency of VCO ~a must not be nil"
       :format-arguments (list name)))
  (if (> 0.0 base-frequency)
      (cl-synthesizer:signal-assembly-error
       :format-control "base-frequency of VCO ~a must be greater than 0: ~a"
       :format-arguments (list name base-frequency)))
  (if (not cv-max)
      (cl-synthesizer:signal-assembly-error
       :format-control "cv-max of VCO ~a must not be nil"
       :format-arguments (list name)))
  (if (not f-max)
      (cl-synthesizer:signal-assembly-error
       :format-control "f-max of VCO ~a must not be nil"
       :format-arguments (list name)))
  (if (> 0.0 f-max)
      (cl-synthesizer:signal-assembly-error
       :format-control "f-max of VCO ~a must be greater than 0: ~a"
       :format-arguments (list name f-max)))
  (let* ((linear-converter
	  (cl-synthesizer-core:linear-converter
	   :input-min 0.0
	   :input-max cv-max
	   :output-min 0.0
	   :output-max f-max))
	 (cv-gain (funcall (getf linear-converter :get-x) base-frequency)))
    (vco-base
     name
     environment
     (lambda (cv)
       (let ((cur-cv (+ (abs cv) cv-gain)))
	 (if (> cur-cv cv-max)
	     (setf cur-cv cv-max))
	 (funcall (getf linear-converter :get-y) cur-cv)))
     :f-max f-max
     :v-peak v-peak)))


