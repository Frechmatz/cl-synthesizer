;;
;;
;; A Wave-File-Writer Device
;;
;;
(in-package :cl-synthesizer-device-wave-file-writer)

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

(defun wave-file-writer (name environment &key channel-count filename (v-peak 5.0))
  "Creates a wave-file-writer device. The device writes files in \"Waveform Audio File\" (\"WAV\") format.
    The function has the following arguments:
  <ul>
    <li>name Name of the device.</li>
    <li>environment The synthesizer environment.</li>
    <li>:channel-count Number of channels.</li>
    <li>:filename The full path of the file to be written.</li>
    <li>:v-peak Optional peak voltage. The inputs of the device will be scaled
	to v-peak. If for example v-peak is set to 20.0 an incoming voltage
	of 5.0 results in a sample value of 5.0 / 20.0 -> 0.25 and an incoming
	voltage of -5.0 results in a sample value of -0.25. The default value
	is 5.0. Incoming voltages will be clipped according to v-peak.</li>
  </ul>
  The device has the following inputs:
  <ul>
      <li>:channel-1 ... :channel-n The sample values of the generated frames
	  are written in order :channel-1 ... :channel-n</li>
  </ul>
  The device has no outputs.
  The actual wave-file is written by the :shutdown function exposed by the device."
  (if (<= channel-count 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: channel-count must be greater than 0: ~a"
       :format-arguments (list name channel-count)))
  (let ((inputs nil) (samples nil))
    (dotimes (i channel-count)
      (push (cl-synthesizer-macro-util:make-keyword "channel" i) inputs))
    ;; inputs are now (:CHANNEL-n ... :CHANNEL-1)
    ;; reverse inputs to (:CHANNEL-1 ... :CHANNEL-n)
    ;; In this order samples will be pushed.
    ;; Final reverse of samples takes place in :shutdown function.
    (setf inputs (reverse inputs))
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
		 (let ((wave (cl-wave:open-wave filename :direction :output)))
		   (cl-wave:set-num-channels wave channel-count)
		   (cl-wave:set-sample-rate wave (getf environment :sample-rate))
		   (cl-wave:set-frames wave (nreverse samples))
		   (cl-wave:close-wave wave)
		   (setf samples nil))))))
