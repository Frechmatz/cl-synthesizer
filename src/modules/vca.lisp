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
