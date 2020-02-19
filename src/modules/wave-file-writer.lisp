(in-package :cl-synthesizer-modules-wave-file-writer)

;;
;; Factory function to be overridden by tests
;;
(defvar *make-writer* (lambda (&rest args)
			(apply #'cl-wave-file-writer:make-writer args)))

(defun input-to-wave (f v-peak)
  ;; convert to -1.0 ... +1.0
  (/ f v-peak))

(defun make-module (name environment &key channel-count filename (v-peak 5.0) (sample-width :16bit))
  "Creates a Wave File Writer module. Writes files in \"Waveform Audio File\" (\"WAV\") format.
    <p>The function has the following arguments:
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
    <li>:sample-width Resolution of samples. One of :8Bit, :16Bit, :24Bit</li> 
  </ul></p>
  <p>The module has the following inputs:
  <ul>
      <li>:channel-1 ... :channel-n The sample values of the generated frames
	  are written in order :channel-1 ... :channel-n</li>
  </ul></p>
  The module has no outputs.
  <p>The recommended way of Wave file generation is to use a Monitor.</p>"
  (if (<= channel-count 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: channel-count must be greater than 0: ~a"
       :format-arguments (list name channel-count)))
  (let ((input-sockets (cl-synthesizer-macro-util:make-keyword-list "channel" channel-count))
	(input-values (make-array channel-count :initial-element nil))
	(opened-wave-writer nil)
	(wave-writer (funcall *make-writer*
		      :filename (merge-pathnames filename (getf environment :home-directory))
		      :channel-count channel-count
		      :sample-width sample-width
		      :sample-rate (floor (getf environment :sample-rate)))))
    ;; input-sockets are now (:CHANNEL-1 ... :CHANNEL-n)
    (let ((inputs nil) (index 0))
      (dolist (input-socket input-sockets)
	(let ((cur-index index))
	  (push (lambda(value) (setf (aref input-values cur-index) value)) inputs)
	  (push input-socket inputs)
	  (setf index (+ 1 index))))
    (list
     :inputs (lambda () inputs)
     :outputs (lambda () nil)
     :update (lambda ()
	       (if (not opened-wave-writer)
		   (progn
		     (setf opened-wave-writer t)
		     (funcall (getf wave-writer :open-file))))
	       (dotimes (i channel-count)
		 (let ((value (aref input-values i)))
		   (if (not value)
		       (setf value 0.0))
		   (funcall (getf wave-writer :write-sample) (input-to-wave value v-peak)))))
     :shutdown (lambda ()
		 (funcall (getf wave-writer :close-file)))))))
