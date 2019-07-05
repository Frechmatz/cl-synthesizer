(in-package :cl-synthesizer-modules-vco)

(defun make-module (name environment &key
				       base-frequency
				       v-peak
				       (cv-lin-hz-v 0.0)
				       (duty-cycle 0.5)
				       (phase-offset 0.0)
				       (f-max 12000.0))
  "Creates a Voltage Controlled Oscillator module with 1V/Octave and linear frequency modulation
   inputs. The oscillator has through-zero support, as on negative frequencies the
   phase will move backwards (in clockwise direction).
   The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:base-frequency The frequency emitted by the oscillator when all frequency control 
           voltages are 0.</li>
	<li>:f-max The maximum frequency of the oscillator. f-max must be greater than 0.</li>
	<li>:cv-lin-hz-v Frequency/Volt setting for the linear frequency modulation input :cv-lin.</li>
	<li>:v-peak Absolute value of the output peak voltage emitted by the oscillator.</li>
	<li>:duty-cycle The duty cycle of the square wave. 0 <= duty-cycle <= 1.</li>
	<li>:phase-offset A phase offset in radians.</li>
    </ul>
    The module has the following inputs:
    <ul>
	<li>:cv-exp Exponential frequency control voltage. For a given base-frequency of 440Hz a
	    control voltage of 1.0 results in a frequency of 880Hz and a control
	    voltage of -1.0 results in a frequency of 220Hz.
	</li>
        <li>:cv-lin Bipolar linear frequency control voltage. Example: If the :cv-lin-hz-v setting
           of the oscillator is 77Hz a :cv-lin input value of 2.0V results in a frequency of 154Hz and 
           a :cv-lin input value of -2.0V results in a frequency of -154Hz.</li>
    </ul>
    The frequency of the oscillator is calculated by adding the frequencies resulting from the
    :cv-lin and :cv-exp inputs. The frequency is clipped according to the :f-max setting.
    The module has the following outputs:
    <ul>
	<li>:sine A sine wave.</li>
	<li>:triangle A triangle wave.</li>
	<li>:saw A saw wave.</li>
	<li>:square A square wave.</li>
    </ul>
    <p>The module exposes the following states via the get-state function:
       <ul>
          <li>:frequency The current frequency of the module.</li>
          <li>:linear-frequency The current linear frequency portion of the module.</li>
          <li>:exponential-frequency The current exponential frequency portion of the module.</li>
          <li>:phase The current phase in radians (0..2PI).</li>
       </ul>
    </p>"
  (declare (type single-float base-frequency f-max v-peak cv-lin-hz-v phase-offset duty-cycle))
  (if (> 0.0 f-max)
      (cl-synthesizer:signal-assembly-error
       :format-control "f-max of VCO ~a must be greater than 0: ~a"
       :format-arguments (list name f-max)))
  (if (>= 0.0 v-peak)
      (cl-synthesizer:signal-assembly-error
       :format-control "v-peak of VCO ~a must be greater than 0: ~a"
       :format-arguments (list name v-peak)))
  (if (< duty-cycle 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "duty-cycle of VCO ~a must not be negative: ~a"
       :format-arguments (list name duty-cycle)))
  (if (< 1.0 duty-cycle)
      (cl-synthesizer:signal-assembly-error
       :format-control "duty-cycle of VCO ~a must not be greater than 1: ~a"
       :format-arguments (list name duty-cycle)))

  (let* ((sample-rate (getf environment :sample-rate))
	 (input-cv-exp nil) (input-cv-lin nil)
	 (cur-frequency 0.0)
	 (cur-lin-frequency 0.0)
	 (cur-exp-frequency 0.0)
	 (cur-phi 0.0)
	 (phase-generator (cl-synthesizer-core:phase-generator sample-rate))
	 (cur-sine-output 1.0)
	 (cur-triangle-output 1.0)
	 (cur-saw-output 1.0)
	 (cur-square-output 1.0)
	 (lin-converter
	  (lambda (cv)
	    (declare (type single-float cv))
	    (* cv cv-lin-hz-v)))
	 (exp-converter
	  (lambda (cv)
	    (declare (type single-float cv))
	    (* base-frequency (expt 2.0 cv)))))
    (flet ((clip-frequency (f)
	     (declare (type single-float f))
	     (if (> (abs f) f-max)
		 (* f-max (signum f))
		 f))
	   (get-frequency (cv-exp cv-lin)
	     (declare (type single-float cv-exp cv-lin))
	     (setf cur-lin-frequency (funcall lin-converter cv-lin))
	     (setf cur-exp-frequency (funcall exp-converter cv-exp))
	     (+ cur-lin-frequency cur-exp-frequency)))
      (let ((inputs (list
		     :cv-exp (lambda(value) (setf input-cv-exp value))
		     :cv-lin (lambda(value) (setf input-cv-lin value))))
	    (outputs (list
		      :sine (lambda() cur-sine-output)
		      :triangle (lambda() cur-triangle-output)
		      :saw (lambda() cur-saw-output)
		      :square (lambda() cur-square-output))))
	(list
	 :inputs (lambda () inputs)
	 :outputs (lambda () outputs)
	 :update (lambda ()
		   (declare (inline
			     cl-synthesizer-core:phase-sine-converter
			     cl-synthesizer-core:phase-saw-converter
			     cl-synthesizer-core:phase-square-converter
			     cl-synthesizer-core:phase-triangle-converter))
		   (if (not input-cv-exp)
		       (setf input-cv-exp 0.0))
		   (if (not input-cv-lin)
		       (setf input-cv-lin 0.0))
		   (let* ((f (clip-frequency (get-frequency input-cv-exp input-cv-lin)))
			  (phi (funcall phase-generator f)))
		     (setf cur-frequency f)
		     (setf cur-phi phi)
		     (setf cur-sine-output
			   (* v-peak (cl-synthesizer-core:phase-sine-converter
				      phi :phase-offset phase-offset)))
		     (setf cur-triangle-output
			   (* v-peak (cl-synthesizer-core:phase-triangle-converter
				      phi :phase-offset phase-offset)))
		     (setf cur-saw-output
			   (* v-peak (cl-synthesizer-core:phase-saw-converter
				      phi :phase-offset phase-offset)))
		     (setf cur-square-output
			   (* v-peak (cl-synthesizer-core:phase-square-converter
				      phi :duty-cycle duty-cycle :phase-offset phase-offset)))))
       :get-state (lambda (key)
		    (cond
		      ((eq key :frequency)
		       cur-frequency)
		      ((eq key :linear-frequency)
		       cur-lin-frequency)
		      ((eq key :exponential-frequency)
		       cur-exp-frequency)
		      ((eq key :phase)
		       cur-phi)
		      (t nil))))))))
  
