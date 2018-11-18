(in-package :cl-synthesizer-modules-wave-file-writer)

(defun wave-writer-float-to-int16 (value)
  (cond
    ((> value 1.0)
     1)
    ((< value -1.0)
     -1)
    (t
     (round (* 32000 value)))))

(defun input-to-wave (f v-peak)
  (wave-writer-float-to-int16
   ;; convert to -1.0 ... +1.0
   (/ f v-peak)))

(defun make-module (name environment &key channel-count filename (v-peak 5.0))
  "Creates a Wave File Writer module. Writes files in \"Waveform Audio File\" (\"WAV\") format.
    The function has the following arguments:
  <ul>
    <li>name Name of the writer.</li>
    <li>environment The synthesizer environment.</li>
    <li>:channel-count Number of channels.</li>
    <li>:filename The relative path of the file to be written. The filename will be concatenated
        with the base path as defined by the :home-directory property of the environment.</li>
    <li>:v-peak Optional peak voltage. The inputs of the module will be scaled
	to v-peak. If for example v-peak is set to 20.0 an incoming voltage
	of 5.0 results in a sample value (which is written into the wave file)  
        of 5.0 / 20.0 -> 0.25 and an incoming voltage of -5.0 results in a sample 
        value of -0.25. The default value is 5.0. Incoming voltages will be clipped 
        according to v-peak.</li>
  </ul>
  The module has the following inputs:
  <ul>
      <li>:channel-1 ... :channel-n The sample values of the generated frames
	  are written in order :channel-1 ... :channel-n</li>
  </ul>
  The module has no outputs.
  The actual wave-file is written by the :shutdown function of the module.
  <p>See also cl-synthesizer-monitor:add-monitor which provides Wave-File-Writing
     without having to add the module and the required patches to the rack.</p>"
  (if (<= channel-count 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: channel-count must be greater than 0: ~a"
       :format-arguments (list name channel-count)))
  (let ((inputs (cl-synthesizer-macro-util:make-keyword-list "channel" channel-count))
	(samples nil))
    ;; inputs are now (:CHANNEL-1 ... :CHANNEL-n)
    (list
     :inputs (lambda () inputs)
     :outputs (lambda () '())
     :get-output (lambda (output) (declare (ignore output)) nil)
     :update (lambda (&rest args)
	       (dolist (input inputs)
		 (let ((value (getf args input)))
		   (if (not value)
		       (setf value 0.0))
		   (push (input-to-wave value v-peak) samples))))
     :shutdown (lambda ()
		 (let ((wave (cl-wave:open-wave
			      (merge-pathnames filename (getf environment :home-directory))
			      :direction :output)))
		   (cl-wave:set-num-channels wave channel-count)
		   (cl-wave:set-sample-rate wave (getf environment :sample-rate))
		   (cl-wave:set-frames wave (nreverse samples))
		   (cl-wave:close-wave wave)
		   (setf samples nil))))))
