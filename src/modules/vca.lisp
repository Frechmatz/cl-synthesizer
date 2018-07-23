;;
;;
;; Voltage Controlled Amplifier
;;
;;
;; Work in progress
;;
;;
(in-package :cl-synthesizer-modules-vca)

(defun vca (name environment)
  (declare (ignore name))
  (let* ((sample-rate (getf environment :sample-rate))
	 (cur-out 0)
	 (converter (cl-synthesizer-core:linear-converter :input-min 0 :input-max 5.0 :output-min 0 :output-max 1))
	 (fn (getf converter :get-y)))
    (declare (ignore sample-rate))
    (list
     :inputs (lambda () '(:input :cv))
     :outputs (lambda () '(:out))
     :get-output (lambda (output)
		   (declare (ignore output))
		   cur-out)
     :update (lambda (&key cv input)
	       (setf cur-out
		     (* (if input input 0)
			(funcall
			 fn
			 (if cv cv 0))))))))
  
