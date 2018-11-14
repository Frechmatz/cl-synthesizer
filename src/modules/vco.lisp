;;
;;
;; Simple Linear/Exponential VCO-Modules
;;
;;

(in-package :cl-synthesizer-modules-base-vco)

(defun make-module (name environment transfer-function &key f-max v-peak (duty-cycle 0.5))
  "Creates a Voltage Controlled Oscillator module. The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>transfer-function A function that converts the frequency control voltage into
	    a frequency. This function is called with the current frequency control voltage and
	    must return a frequency. Frequencies greater than f-max will be clipped. Negative
	    frequencies will be clipped to 0Hz.</li>
	<li>:f-max The maximum frequency of the oscillator. f-max must be greater than 0.</li>
	<li>:v-peak Absolute value of the maximal voltage (positive/negative) emitted by the oscillator.</li>
	<li>:duty-cycle The duty cycle of the square wave. 0 >= duty-cycle <= 1.</li>
    </ul>
    The module has the following inputs:
    <ul>
	<li>:cv Frequency control voltage.</li>
    </ul>
    The module has the following outputs:
    <ul>
	<li>:sine A sine wave.</li>
	<li>:triangle A triangle wave.</li>
	<li>:saw A saw wave.</li>
	<li>:square A square wave.</li>
    </ul>"
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
  (if (not duty-cycle)
      (cl-synthesizer:signal-assembly-error
       :format-control "duty-cycle of VCO ~a must not be nil"
       :format-arguments (list name)))
  (if (< duty-cycle 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "duty-cycle of VCO ~a must not be negative: ~a"
       :format-arguments (list name duty-cycle)))
  (if (< 1.0 duty-cycle)
      (cl-synthesizer:signal-assembly-error
       :format-control "duty-cycle of VCO ~a must not be greater than 1: ~a"
       :format-arguments (list name duty-cycle)))
  (let* ((sample-rate (getf environment :sample-rate))
	 (cur-frequency 0)
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
       :get-cur-frequency (lambda() cur-frequency)
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
		   (setf cur-frequency f)
		   (setf cur-sine-output (* v-peak (cl-synthesizer-core:phase-sine-converter phi)))
		   (setf cur-triangle-output (* v-peak (cl-synthesizer-core:phase-triangle-converter phi)))
		   (setf cur-saw-output (* v-peak (cl-synthesizer-core:phase-saw-converter phi)))
		   (setf cur-square-output (* v-peak (cl-synthesizer-core:phase-square-converter
						      phi :duty-cycle duty-cycle)))))))))


(in-package :cl-synthesizer-modules-exponential-vco)

(defun make-module (name environment &key base-frequency f-max v-peak (duty-cycle 0.5))
  "Creates a Voltage Controlled Oscillator module with 1V/Octave characteristic.
   The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:base-frequency The frequency emitted by the oscillator at a frequency control voltage of 0.</li>
	<li>:f-max The maximum frequency of the oscillator. f-max must be greater than 0.</li>
	<li>:v-peak Absolute value of the maximal voltage (positive/negative) emitted by the oscillator.</li>
	<li>:duty-cycle The duty cycle of the square wave. 0 >= duty-cycle <= 1.</li>
    </ul>
    The module has the following inputs:
    <ul>
	<li>:cv Frequency control voltage. For a given base-frequency of 440Hz a
	    control voltage of 1.0 results in a frequency of 880Hz and a control
	    voltage of -1.0 results in a frequency of 220Hz.
	</li>
    </ul>
    The module has the following outputs:
    <ul>
	<li>:sine A sine wave.</li>
	<li>:triangle A triangle wave.</li>
	<li>:saw A saw wave.</li>
	<li>:square A square wave.</li>
    </ul>"
  (if (not base-frequency)
      (cl-synthesizer:signal-assembly-error
       :format-control "base-frequency of VCO ~a must not be nil"
       :format-arguments (list name)))
  (if (> 0.0 base-frequency)
      (cl-synthesizer:signal-assembly-error
       :format-control "base-frequency of VCO ~a must be greater than 0: ~a"
       :format-arguments (list name base-frequency)))
  (cl-synthesizer-modules-base-vco::make-module
   name
   environment
   (lambda (cv)
     (* base-frequency (expt 2 cv)))
   :f-max f-max
   :v-peak v-peak
   :duty-cycle duty-cycle))

(in-package :cl-synthesizer-modules-linear-vco)

(defun make-module (name environment &key base-frequency f-max v-peak cv-max (duty-cycle 0.5))
  "Creates a Voltage Controlled Oscillator module with linear characteristic.
   The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:cv-max The frequency control voltage which represents the maximum frequency of the oscillator.</li>
	<li>:base-frequency The frequency emitted by the oscillator at a frequency control voltage of 0.</li>
	<li>:f-max The maximum frequency of the oscillator. f-max must be greater than 0.</li>
	<li>:v-peak Absolute value of the maximal voltage (positive/negative) emitted by the oscillator.</li>
	<li>:duty-cycle The duty cycle of the square wave. 0 >= duty-cycle <= 1.</li>
    </ul>
    The module has the following inputs:
    <ul>
	<li>:cv Frequency control voltage. For frequency calculation the absolute value
	of the control voltage is used. The control voltage is clipped at :cv-max.</li>
    </ul>
    The module has the following outputs:
    <ul>
	<li>:sine A sine wave.</li>
	<li>:triangle A triangle wave.</li>
	<li>:saw A saw wave.</li>
	<li>:square A square wave.</li>
    </ul>"
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
    (cl-synthesizer-modules-base-vco::make-module
     name
     environment
     (lambda (cv)
       (let ((cur-cv (+ (abs cv) cv-gain)))
	 (if (> cur-cv cv-max)
	     (setf cur-cv cv-max))
	 (funcall (getf linear-converter :get-y) cur-cv)))
     :f-max f-max
     :v-peak v-peak
     :duty-cycle duty-cycle)))


