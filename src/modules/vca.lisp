;;
;;
;; Voltage Controlled Amplifier
;;
;;
;; Work in progress
;;
;;
(in-package :cl-synthesizer-modules-vca)

(defun vca (name environment &key max-amplification max-amplification-cv (initial-gain 0.0))
  (declare (ignore environment name))
  (let ((cur-out-linear 0)
	(cur-out-exponential 0)
	(linear-amplification-fn
	 (getf
	  (cl-synthesizer-core:linear-converter
	   :input-min 0.0
	   :input-max max-amplification-cv
	   :output-min 0.0
	   :output-max max-amplification)
	  :get-y))
	(exponential-amplification-fn
	 (lambda (cv)
	   ;; multiply with (/ cv max-amplification-cv) due to (expt 2 0) => 1.0
	   ;; on CV = 0.0 amplification factor must be 0.0 
	   (* max-amplification
	      (* (/ cv max-amplification-cv)
		 (/ (expt 2 cv) (expt 2 max-amplification-cv)))))))
    (list
     :inputs (lambda () '(:input :cv :gain))
     :outputs (lambda () '(:output-linear :output-exponential))
     :get-output (lambda (output)
		   (cond 
		     ((eq output :output-linear)
		      cur-out-linear)
		     (t
		      cur-out-exponential)))
     :update (lambda (&key cv input gain)
	       (if (not cv)
		   (setf cv 0.0))
	       (if (not gain)
		   (setf gain 0.0))
	       (if (not input)
		   (setf input 0.0))
	       (setf cv (+ cv initial-gain gain))
	       (if (> 0.0 cv)
		   (setf cv 0.0))
	       (if (> cv max-amplification-cv)
		   (setf cv max-amplification-cv))
	       ;; TODO more clipping, for example cv > max-amplification-cv and negative cv
	       (setf cur-out-linear (* input (funcall linear-amplification-fn cv)))
	       (setf cur-out-exponential (* input (funcall exponential-amplification-fn cv)))))))


(defun vca-core ()
  (flet
      ((validate-cv (cv)
	 (if (> cv 10.0)
	     (cl-synthesizer:signal-invalid-arguments-error
	      :format-control "Input of VCA-CORE must not be greater than 10.0"
	      :format-arguments (list cv)))
	 (if (> 0.0 cv)
	     (cl-synthesizer:signal-invalid-arguments-error
	      :format-control "Input of VCA-CORE must not be smaller than 0.0"
	      :format-arguments (list cv)))))
    (list
     :exponential
     (lambda (cv)
       #|
       Matlab-Plot:
       maxCV = 10.0;
       cv = 0:0.01:maxCV;
       y = (pow2 (cv) - 1.0) / pow2(maxCV);
       plot (cv,y);
       |#
       (validate-cv cv) 
       (/ (+ (expt 2 cv) -1.0) (expt 2 10.0)))
     :linear
     (lambda (cv)
       #|
       Matlab-Plot:
       maxCV = 10.0;
       cv = 0:0.01:maxCV;
       y = cv / maxCV;
       plot (cv,y);
       |#
       (validate-cv cv) 
       (/ cv 10.0)))))
  
       
(defun vca-ng (name
	       environment
	       &key
		 cv-max ;; Input value of cv socket indicating an amplification of 1.0
		 (cv-initial-gain 0.0))
  (declare (ignore environment name))
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (if (> 0.0 cv-max)
      (cl-synthesizer:signal-assembly-error
       :format-control "CV-MAX must not be smaller than 0.0"
       :format-arguments (list cv-max)))
  (let* ((cur-out-linear 0)
	 (cur-out-exponential 0)
	 (vca-core (vca-core))
	 (vca-core-lin (getf vca-core :linear))
	 (vca-core-exp (getf vca-core :exponential))
	 (cv-converter (getf 
			(cl-synthesizer-core:linear-converter
			 :input-min 0.0
			 :input-max cv-max
			 :output-min 0.0
			 :output-max 10.0)
			:get-y)))
    (list
     :inputs (lambda () '(:input :cv :cv-gain))
     :outputs (lambda () '(:output-linear :output-exponential
			   :input-normalized
			   :cv-original
			   :cv-unclipped
			   :cv-clipped
			   ))
     :get-output (lambda (output)
		   (cond 
		     ((eq output :output-linear)
		      cur-out-linear)
		     ((eq output :output-exponential)
		      cur-out-exponential)
		     (t
		      (error "Invalid output requested from vca"))))
     :update (lambda (&key cv input cv-gain)
	       (declare (optimize (debug 3) (speed 0) (space 0)))
	       (if (not input)
		   (setf input 0.0))
	       (if (not cv)
		   (setf cv 0.0))
	       (if (not cv-gain)
		   (setf cv-gain 0.0))
	       (setf cv (+ cv cv-initial-gain cv-gain))
	       (if (> cv cv-max)
		   (setf cv cv-max))
	       (if (> 0.0 cv)
		   (setf cv 0.0))
	       (setf cv (funcall cv-converter cv))
	       (setf cur-out-linear (* input (funcall vca-core-lin cv)))
	       (setf cur-out-exponential (* input (funcall vca-core-exp cv)))
	       ))))

