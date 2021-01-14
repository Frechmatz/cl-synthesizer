;;
;;
;; Voltage Controlled Amplifier
;;
;;

(in-package :cl-synthesizer-modules-vca)

(defun make-module (name
	    environment
	    &key
	      cv-max
	      (initial-gain 0.0)
	      (exponential nil))
  "Creates a Voltage Controlled Amplifier/Attenuator module. The VCA multiplies an
    incoming signal with a factor of 0..1. <p>The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:cv-max The value of the effective amplification control voltage that represents the maximum
	    amplification of 1.0.</li>
	<li>:initial-gain An offset that is added to the amplification control voltage.</li>
        <li>:exponential If t then the amplification will have an exponential characteristic.</li>
    </ul></p>
    <p>The module has the following inputs:
    <ul>
	<li>:cv Amplification control voltage.</li>
	<li>:input Input signal to be amplified.</li>
	<li>:gain An offset that is added to the amplification control voltage.</li>
    </ul>
    The effective amplification voltage is v = :cv + :gain + :initial-gain, where 0.0 <= v <= :cv-max.</p>
    <p>The module has the following outputs:
    <ul>
	<li>:output Amplified input signal.</li>
    </ul></p>"
  (declare (ignore environment name))
  (let ((cv-max (coerce cv-max 'single-float))
	(initial-gain (coerce initial-gain 'single-float)))
    (declare (type single-float cv-max initial-gain))
    ;; (declare (optimize (debug 3) (speed 0) (space 0)))
    (if (> 0.0 cv-max)
	(cl-synthesizer:signal-assembly-error
	 :format-control "CV-MAX must not be smaller than 0.0"
	 :format-arguments (list cv-max)))
    (let* ((cur-out 0)
	   (input-signal nil) (input-cv nil) (input-gain nil)
	   (transfer-fn nil))
      (if (not exponential)
	  ;; Linear transfer
	  (setf transfer-fn (lambda(cv)
			      (declare (type single-float cv))
			      (/ cv cv-max)))
	  ;; Exponential transfer
	  (setf transfer-fn (lambda(cv)
			      (declare (type single-float cv))
			      (cl-synthesizer-core:normalized-exp (/ cv cv-max)))))
      (let ((inputs (list
		     :input (lambda(value) (setf input-signal value))
		     :cv (lambda(value) (setf input-cv value))
		     :gain (lambda(value) (setf input-gain value))))
	    (outputs (list
		      :output (lambda() cur-out))))
	(list
	 :inputs (lambda () inputs)
	 :outputs (lambda () outputs)
	 :update (lambda ()
		   (if (not input-signal)
		       (setf input-signal 0.0))
		   (if (not input-cv)
		       (setf input-cv 0.0))
		   (if (not input-gain)
		       (setf input-gain 0.0))
		   (setf input-cv (+ input-cv initial-gain input-gain))
		   (if (> input-cv cv-max)
		       (setf input-cv cv-max))
		   (if (> 0.0 input-cv)
		       (setf input-cv 0.0))
		   (setf cur-out (* input-signal (funcall transfer-fn input-cv)))))))))

