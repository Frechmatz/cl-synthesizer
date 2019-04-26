;;
;;
;; Voltage Controlled Amplifier
;;
;;

(in-package :cl-synthesizer-modules-vca)


(defun vca-core ()
  "Maps 0..10 to 0..1 with linear and exponential characteristic"
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
  
       
(defun make-module (name
	    environment
	    &key
	      cv-max
	      (initial-gain 0.0))
  "Creates a Voltage Controlled Amplifier/Attenuator module. The VCA multiplies an
    incoming signal with a factor of 0..1. The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:cv-max The value of the effective amplification control voltage that represents the maximum
	    amplification of 1.0.</li>
	<li>:initial-gain An offset that is added to the amplification control voltage.</li>
    </ul>
    The module has the following inputs:
    <ul>
	<li>:cv Amplification control voltage.</li>
	<li>:input Input signal to be amplified. The amplitude of this voltage is
	    unknown to the VCA. It can have any value.</li>
	<li>:gain An offset that is added to the amplification control voltage.</li>
    </ul>
    The effective amplification voltage is v = :cv + :gain + :initial-gain, where 0.0 <= v <= :cv-max.
    The module has the following outputs:
    <ul>
	<li>:output-linear Amplified input signal with linear amplification characteristic.</li>
	<li>:output-exponential Amplified input signal with exponential amplification characteristic.</li>
    </ul>"
  (declare (ignore environment name))
  ;; (declare (optimize (debug 3) (speed 0) (space 0)))
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
			:get-y))
	 (input-input nil) (input-cv nil) (input-gain nil))
    (let ((inputs (list
		   :input (lambda(value) (setf input-input value))
		   :cv (lambda(value) (setf input-cv value))
		   :gain (lambda(value) (setf input-gain value))))
	  (outputs (list
		    :output-linear (lambda() cur-out-linear)
		    :output-exponential (lambda() cur-out-exponential))))
    (list
     :inputs (lambda () inputs)
     :outputs (lambda () outputs)
     :update (lambda ()
	       (if (not input-input)
		   (setf input-input 0.0))
	       (if (not input-cv)
		   (setf input-cv 0.0))
	       (if (not input-gain)
		   (setf input-gain 0.0))
	       (setf input-cv (+ input-cv initial-gain input-gain))
	       (if (> input-cv cv-max)
		   (setf input-cv cv-max))
	       (if (> 0.0 input-cv)
		   (setf input-cv 0.0))
	       (setf input-cv (funcall cv-converter input-cv))
	       (setf cur-out-linear (* input-input (funcall vca-core-lin input-cv)))
	       (setf cur-out-exponential (* input-input (funcall vca-core-exp input-cv))))))))

