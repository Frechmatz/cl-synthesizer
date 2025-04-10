(in-package :cl-synthesizer-modules-vco)

(defun make-module (name environment &key
				       base-frequency
				       v-peak
				       (cv-lin-hz-v 0.0)
				       (duty-cycle 0.5)
				       (phase-offset 0.0)
				       (f-max 12000.0)
				       (wave-forms nil)
				       (sync-threshold 2.5))
  "Creates a Voltage Controlled Oscillator module with 1V/Octave and linear frequency modulation
   inputs. The oscillator has through-zero support.
   <p>The function has the following parameters:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:base-frequency The frequency emitted by the oscillator when all frequency control 
           voltages are 0.</li>
	<li>:f-max Absolute value of the maximum frequency of the oscillator. f-max must be greater than 0.</li>
	<li>:cv-lin-hz-v Frequency/Volt setting for the linear frequency modulation input :cv-lin.</li>
	<li>:v-peak Absolute value of the output peak voltage emitted by the oscillator.</li>
	<li>:duty-cycle The duty cycle of the square wave. 0 <= duty-cycle <= 1.</li>
	<li>:phase-offset Phase offset in radians.</li>
        <li>:wave-forms A list of keywords that declares the wave forms that are to be exposed by 
          the module. Each keyword must be one of :sine, :saw, :triangle or :square. By default
          the module exposes all wave forms. Declaring the required wave forms is highly recommended
          as this can improve the execution speed of the module significantly (up to 50%).</li>
        <li>:sync-threshold Minimum value of the :sync input that indicates 
            that a phase reset is to be applied.</li>
    </ul></p>
    <p>The module has the following inputs:
    <ul>
	<li>:cv-exp Exponential frequency control voltage. For a given base-frequency of 440Hz a
	    control voltage of 1.0 results in a frequency of 880Hz and a control
	    voltage of -1.0 results in a frequency of 220Hz.
	</li>
        <li>:cv-lin Bipolar linear frequency control voltage. Example: If the :cv-lin-hz-v setting
           of the oscillator is 77Hz a :cv-lin input value of 2.0V results in a frequency of 154Hz and 
           a :cv-lin input value of -2.0V results in a frequency of -154Hz.</li>
        <li>:sync Indicator if the phase is to be reset. See also :sync-threshold constructor parameter.</li>
    </ul>
    The frequency of the oscillator is calculated by adding the frequencies resulting from the
    :cv-lin and :cv-exp inputs. The frequency is clipped according to the :f-max setting.</p>
    <p>The module has the following outputs (depending on the :wave-forms parameter):
    <ul>
	<li>:sine A sine wave.</li>
	<li>:triangle A triangle wave.</li>
	<li>:saw A saw wave.</li>
	<li>:square A square wave.</li>
    </ul></p>
    <p>The module exposes the following states:
       <ul>
          <li>:frequency The current frequency of the module.</li>
          <li>:linear-frequency The current linear frequency portion of the module.</li>
          <li>:exponential-frequency The current exponential frequency portion of the module.</li>
          <li>:phase The current phase in radians (0..2PI).</li>
       </ul>
    </p>"
  (let ((base-frequency (coerce base-frequency 'single-float))
	(f-max (coerce f-max 'single-float))
	(v-peak (coerce v-peak 'single-float))
	(cv-lin-hz-v (coerce cv-lin-hz-v 'single-float))
	(phase-offset (coerce phase-offset 'single-float))
	(duty-cycle (coerce duty-cycle 'single-float))
	(sync-threshold (coerce sync-threshold 'single-float)))
    (declare (type single-float base-frequency f-max v-peak cv-lin-hz-v phase-offset duty-cycle sync-threshold))
    (if (not wave-forms)
	(setf wave-forms (list :sine :saw :square :triangle)))
    (if (> 0.0 f-max)
	(error
	 'cl-synthesizer:assembly-error
	 :format-control "f-max of VCO '~a' must be greater than 0: '~a'"
	 :format-arguments (list name f-max)))
    (if (>= 0.0 v-peak)
	(error
	 'cl-synthesizer:assembly-error
	 :format-control "v-peak of VCO '~a' must be greater than 0: '~a'"
	 :format-arguments (list name v-peak)))
    (if (< duty-cycle 0)
	(error
	 'cl-synthesizer:assembly-error
	 :format-control "duty-cycle of VCO '~a' must not be negative: '~a'"
	 :format-arguments (list name duty-cycle)))
    (if (< 1.0 duty-cycle)
	(error
	 'cl-synthesizer:assembly-error
	 :format-control "duty-cycle of VCO '~a' must not be greater than 1: '~a'"
	 :format-arguments (list name duty-cycle)))

    (let* ((sample-rate (getf environment :sample-rate))
	   (input-cv-exp nil) (input-cv-lin nil) (input-sync nil)
	   (cur-frequency 0.0)
	   (cur-lin-frequency 0.0)
	   (cur-exp-frequency 0.0)
	   (cur-phi 0.0)
	   (cur-sine-output 1.0)
	   (cur-triangle-output 1.0)
	   (cur-saw-output 1.0)
	   (cur-square-output 1.0)
	   (initial-sync t)
	   (lin-converter
	    (lambda (cv)
	      (declare (type single-float cv))
	      (* cv cv-lin-hz-v)))
	   (exp-converter
	    (lambda (cv)
	      (declare (type single-float cv))
	      (* base-frequency (expt 2.0 cv)))))

      (multiple-value-bind (phase-generator phase-reset)
	  (cl-synthesizer-util:phase-generator sample-rate)
	
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
			 :cv-exp (list
				  :set (lambda(value) (setf input-cv-exp value))
				  :get (lambda() input-cv-exp))
			 :cv-lin (list
				  :set (lambda(value) (setf input-cv-lin value))
				  :get (lambda() input-cv-lin))
			 :sync (list
				:set (lambda(value) (setf input-sync value))
				:get (lambda() input-sync))))
		(outputs nil)
		(update-functions (make-array (length wave-forms))))
	    (let ((index nil) (added-wave-forms nil))
	      (dolist (wave-form wave-forms)
		(setf index (if (not index) 0 (+ index 1)))
		(if (find wave-form added-wave-forms)
		    (error
		     'cl-synthesizer:assembly-error
		     :format-control "VCO '~a': wave-forms must be unique: '~a'"
		     :format-arguments (list name wave-forms)))
		(push wave-form added-wave-forms)
		(cond
		  ((eq wave-form :sine)
		   (push (list :get (lambda() cur-sine-output)) outputs)
		   (push :sine outputs)
		   (setf (elt update-functions index)
			 (lambda()
			   (declare (inline cl-synthesizer-util:phase-sine-converter))
			   (setf cur-sine-output
				 (* v-peak
				    (cl-synthesizer-util:phase-sine-converter
				     cur-phi phase-offset))))))
		  ((eq wave-form :saw)
		   (push (list :get (lambda() cur-saw-output)) outputs)
		   (push :saw outputs)
		   (setf (elt update-functions index)
			 (lambda()
			   (declare (inline cl-synthesizer-util:phase-saw-converter))
			   (setf cur-saw-output
				 (* v-peak (cl-synthesizer-util:phase-saw-converter
					    cur-phi phase-offset))))))
		  ((eq wave-form :square)
		   (push (list :get (lambda() cur-square-output)) outputs)
		   (push :square outputs)
		   (setf (elt update-functions index)
			 (lambda()
			   (declare (inline cl-synthesizer-util:phase-square-converter))
			   (setf cur-square-output
				 (* v-peak (cl-synthesizer-util:phase-square-converter
					    cur-phi phase-offset duty-cycle))))))
		  ((eq wave-form :triangle)
		   (push (list :get (lambda() cur-triangle-output)) outputs)
		   (push :triangle outputs)
		   (setf (elt update-functions index)
			 (lambda()
			   (declare (inline cl-synthesizer-util:phase-triangle-converter))
			   (setf cur-triangle-output
				 (* v-peak (cl-synthesizer-util:phase-triangle-converter
					    cur-phi phase-offset))))))
		  (t
		   (error
		    'cl-synthesizer:assembly-error
		    :format-control "Invalid wave-form identifier '~a' passed to VCO '~a'"
		    :format-arguments (list wave-form name))))))
	    (list
	     :inputs (lambda() inputs)
	     :outputs (lambda() outputs)
	     :update (lambda ()
		       (if (not input-cv-exp)
			   (setf input-cv-exp 0.0))
		       (if (not input-cv-lin)
			   (setf input-cv-lin 0.0))
		       (if (not input-sync)
			   (setf input-sync 0.0))
		       (let ((f (clip-frequency (get-frequency input-cv-exp input-cv-lin)))
			     (phi nil))
			 (if (or (<= sync-threshold input-sync) initial-sync)
			     (progn
			       (setf phi (funcall phase-reset))
			       (setf initial-sync nil))
			     (progn
			       (setf phi (funcall phase-generator f))))
			 (setf cur-frequency f)
			 (setf cur-phi phi)
			 (dotimes (i (length update-functions))
			   (funcall (elt update-functions i)))))
	     :state (lambda (key)
			  (cond
			    ((eq key :frequency)
			     cur-frequency)
			    ((eq key :linear-frequency)
			     cur-lin-frequency)
			    ((eq key :exponential-frequency)
			     cur-exp-frequency)
			    ((eq key :phase)
			     cur-phi)
			    (t nil))))))))))

