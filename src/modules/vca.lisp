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


(defun vca-ng (name environment &key
				  ;;				  input-max ;; Referenzgroesse für Verstärkung
				  ;; Bedeutung; Input(1.0) => Output-Max bei CV = CV-Max 
				  output-max ;; max output voltage 
				  cv-max ;; Value of cv input indicating max amplification
				  (cv-initial-gain 0.0)
				  )
  (declare (ignore environment name))
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let* (
	(cur-out-linear 0)
	(cur-out-exponential 0)
	(lin-fn (lambda (input cv)
		  (* input output-max (/ cv cv-max))))
	(exp-fn (lambda (input cv)
		  (* input output-max (/ (expt 2 cv) (expt 2 cv-max)))))

#|	
	   (let ((max-amplification (/ output-max input-max)))
	     (lambda (input cv)
	       ;; multiply with (/ cv max-amplification-cv) due to (expt 2 0) => 1.0
	       ;; on CV = 0.0 amplification factor must be 0.0 
;;	       (* (/ input input-max) 
	       (* input
		  (* max-amplification
		     (* (/ cv cv-max)
			(/ (expt 2 cv) (expt 2 cv-max))))))))))
|#
	)
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
	       (if (not cv)
		   (setf cv 0.0))
	       (if (not cv-gain)
		   (setf cv-gain 0.0))
	       (setf cv (+ cv cv-initial-gain cv-gain))
	       (if (not input)
		   (setf input 0.0))
	       (if (> cv cv-max)
		   (setf cv cv-max))
	       (if (> 0.0 cv)
		   (setf cv 0.0))
	       ;; TODO more clipping, for example cv > max-amplification-cv and negative cv
	       (setf cur-out-linear (funcall lin-fn input cv))
;;	       (if (> cur-out-linear output-max)
;;		   (setf cur-out-linear output-max))
	       (setf cur-out-exponential (funcall exp-fn input cv))
;;	       (if (>= 2.5 input)
;;		   (break))
;;	       (if (> cur-out-exponential output-max)
;;		   (setf cur-out-exponential output-max))
	       

	       ))))

