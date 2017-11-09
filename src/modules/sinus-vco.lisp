;;
;;
;; Stand-alone sine VCO. Does not depend on cl-synthesizer-core
;; DEPRECATED
;;
(in-package :cl-synthesizer-modules-sinus-vco)

(defun sinus-vco (environment &key (f_0 440) (f_delta 50))
  (declare (optimize (debug 3) (speed 0) (space 0)))
  ;; (break)
  (if (>= f_delta f_0)
      (error "f_0 must be greater than f_delta"))
  (let ((sample-rate (getf environment :sample-rate))
	(curPhi 0)
	(current-output (* 1.0 cl-synthesizer-modules-constants:+V-PEAK+))
	(inputs (list :v_in))
	(outputs (list :out)))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () inputs)
     :outputs (lambda () outputs)
     :get-output (lambda (output)
		   (declare (ignore output))
		   current-output)
     :update (lambda (&key (v_in 0))
	       (declare (optimize (debug 3) (speed 0) (space 0)))
	       ;; (break)
	       (labels ((get-cur-frequency (v_in)
			  (declare (optimize (debug 3) (speed 0) (space 0)))
			  (+ f_0 (* (/ v_in cl-synthesizer-modules-constants:+V-PEAK+) f_delta)))
			(get-delta-phi (cur-frequency)
			  (let ((deltaPhi (/ (* 2 PI cur-frequency) sample-rate)))
			    deltaPhi)))
		 ;; Calc output
		 (let ((deltaPhi (get-delta-phi (get-cur-frequency v_in))))
		   (setf current-output (* (sin curPhi) cl-synthesizer-modules-constants:+V-PEAK+))
		   (setf curPhi (+ curPhi deltaPhi))))))))

