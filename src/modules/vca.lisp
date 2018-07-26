;;
;;
;; Voltage Controlled Amplifier
;;
;;
;; Work in progress
;;
;;
(in-package :cl-synthesizer-modules-vca)

(defun vca (name environment &key (input-min 0) (input-max 5.0) (output-min 0) (output-max 1))
  (declare (ignore name))
  (let* ((sample-rate (getf environment :sample-rate))
	 (cur-out 0)
	 (converter (cl-synthesizer-core:linear-converter
		     :input-min input-min
		     :input-max input-max
		     :output-min output-min
		     :output-max output-max))
	 (fn (getf converter :get-y)))
    (declare (ignore sample-rate))
    (list
     :inputs (lambda () '(:input :cv))
     :outputs (lambda () '(:output))
     :get-output (lambda (output)
		   (declare (ignore output))
		   cur-out)
     :update (lambda (&key cv input)
	       (setf cur-out
		     (* (if input input 0)
			(funcall
			 fn
			 (if cv cv 0))))))))

#|
(defun vca-exponential (name environment &key (cv-max 5.0))
  (declare (ignore name))
  (let* ((sample-rate (getf environment :sample-rate))
	 (cur-out 0)
	 (fn (lambda (cv input)
	       (* input (/ (expt 2 cv) (expt 2 cv-max))))))
    (declare (ignore sample-rate))
    (list
     :inputs (lambda () '(:input :cv))
     :outputs (lambda () '(:output))
     :get-output (lambda (output)
		   (declare (ignore output))
		   cur-out)
     :update (lambda (&key cv input)
	       ;; todo clip negative cv
	       (if (not cv)
		   (setf cv 0.0))
	       (if (not input)
		   (setf input 0.0))
	       (setf cur-out (funcall fn cv input))))))
|#

(defun vca-ng (name environment &key max-amplification max-amplification-cv)
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
	   (* max-amplification (/ (expt 2 cv) (expt 2 max-amplification-cv))))))
    (list
     :inputs (lambda () '(:input :cv))
     :outputs (lambda () '(:output-linear :output-exponential))
     :get-output (lambda (output)
		   (cond 
		     ((eq output :output-linear)
		      cur-out-linear)
		     (t
		      cur-out-exponential)))
     :update (lambda (&key cv input)
	       ;; todo clip negative cv
	       (if (not cv)
		   (setf cv 0.0))
	       (if (not input)
		   (setf input 0.0))
	       ;; TODO more clipping, for example cv > max-amplification-cv and negative cv
	       (setf cur-out-linear (* input (funcall linear-amplification-fn cv)))
	       (setf cur-out-exponential (* input (funcall exponential-amplification-fn cv)))))))
