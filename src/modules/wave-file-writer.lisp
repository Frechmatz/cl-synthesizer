(in-package :cl-synthesizer-modules-wave-file-writer)


;;
;; Helper functions for RIFF/Binary writing
;; Copied from Ryan Kings cl-wave library https://github.com/RyanTKing/cl-wave
;;
(defun write-sint (stream sint bytes)
  "Writes a signed integer to the stream with the specified number of bytes."
  (when (< sint 0) (incf sint (expt 2 (* bytes 8))))
  (loop for n below (* bytes 8) by 8 do (write-byte (ldb (byte 8 n) sint) stream)))

(defun write-uint (stream uint bytes)
  "Writes an unsigned integer to the stream with the specified number of bytes."
  (loop for n below (* bytes 8) by 8 do (write-byte (ldb (byte 8 n) uint) stream)))

(defun write-tag (stream tag)
  "Writes a 4-character ASCII tag to the stream."
  (loop for ch across tag do (write-byte (char-code ch) stream)))


(defmacro clip-value (value)
  `(cond
    ((> ,value 1.0)
     1.0)
    ((< ,value -1.0)
     -1.0)
    (t ,value)))

;;
;; Regarding final clipping (after round) see also
;; https://stackoverflow.com/questions/54548304/subtlety-in-converting-doubles-to-a-sound-byte-output
;; We want to allow both +1.0 and -1.0 is input values
;;

(defun value-to-8bit-unsigned (value)
  "value: -1.0 ... 1.0"
  (setf value (clip-value value))
  (setf value (round (* 128 value)))
  (setf value (+ 128 value))
  (cond
    ((< 255 value)
     255)
    ((< value 0)
     0)
    (t
     value)))

(defun value-to-16bit-signed (value)
  "value: -1.0 ... 1.0"
  (setf value (clip-value value))
  (setf value (round (* 32768 value)))
  (cond
    ((< 32767 value)
     32767)
    ((< value -32768)
     -32768)
    (t
     value)))

(defun value-to-24bit-signed (value)
  "value: -1.0 ... 1.0"
  (setf value (clip-value value))
  (setf value (round (* 8388608 value)))
  (cond
    ((< 8388607 value)
     8388607)
    ((< value -8388608)
     -8388608)
    (t
     value)))

;;
;; Streaming Wave-File-Writer
;;

(defun make-writer (&key filename channel-count sample-rate (sample-width :16Bit))
  "Creates a streaming Wave-File output writer. Returns a property list with the following
   keys:
   <ul>
      <li>:open-file A function that opens the file.</li>
      <li>:close-file A function that closes the file.</li>
      <li>:write-sample A function that writes a sample. A sample is a signed 16 Bit integer (−32.768 ... 32.767).</li>
   </ul>"
  (let ((sample-width-mapping
	 (list
	  :8Bit
	  (list
	   :sample-width-bytes 1
	   ;; Unsigned representation for 8 bit waves
	   ;; https://en.wikipedia.org/wiki/WAV
	   ;; https://stackoverflow.com/questions/44415863/what-is-the-byte-format-of-an-8-bit-monaural-wav-file
	   :convert #'value-to-8bit-unsigned)
	  :16Bit
	  (list
	   :sample-width-bytes 2
	   :convert #'value-to-16bit-signed)
	  :24Bit
	  (list
	   :sample-width-bytes 3
	   :convert #'value-to-24bit-signed))))
    (let* ((sample-count 0)
	   (file-output-stream)
	   (sample-mapping (getf sample-width-mapping sample-width))
	   (sample-width-bytes (getf sample-mapping :sample-width-bytes))
	   (convert-sample (getf sample-mapping :convert)))
      (labels ((open-file ()
		 (format t "~%Open file ~a~%" filename)
		 (setf file-output-stream
		       (open
			filename
			:element-type 'unsigned-byte
			:direction :io
			:if-exists :supersede
			:if-does-not-exist :create)))
	       (close-file ()
		 (if file-output-stream
		     (progn
		       (format t "~%Close file ~a~%" filename)
		       (close file-output-stream)
		       (setf file-output-stream nil))))
	       (write-sample (value)
		 (write-sint
		  file-output-stream
		  (funcall convert-sample value)
		  sample-width-bytes))
	       (get-fmt-chunk-size (number-of-samples)
		 (declare (ignore number-of-samples))
		 16)
	       (get-data-chunk-size (number-of-samples)
		 (* number-of-samples sample-width-bytes))
	       (get-riff-chunk-size (number-of-samples)
		 (+ 4 (get-data-chunk-size number-of-samples) (get-fmt-chunk-size number-of-samples)))
	       (write-riff-chunk (number-of-samples)
		 (let ((riff-size (get-riff-chunk-size number-of-samples)))
		   (write-tag file-output-stream "RIFF")
		   (write-uint file-output-stream riff-size 4)
		   (write-tag file-output-stream "WAVE")))
	       (write-format-chunk (number-of-samples)
		 (let ((compression-code 1) ;; PCM
		       ;; https://de.wikipedia.org/wiki/RIFF_WAVE
		       (byte-rate (* sample-rate channel-count sample-width-bytes)))
		   (write-tag file-output-stream "fmt ")
		   (write-uint file-output-stream (get-fmt-chunk-size number-of-samples) 4)
		   (write-uint file-output-stream compression-code 2)
		   (write-uint file-output-stream channel-count 2)
		   (write-uint file-output-stream sample-rate 4)
		   (write-uint file-output-stream byte-rate 4)
		   (write-uint file-output-stream sample-width-bytes 2)
		   (write-uint file-output-stream (* sample-width-bytes 8) 2)))
	       (write-data-chunk (number-of-samples)
		 (let ((data-size (get-data-chunk-size number-of-samples)))
		   (write-tag file-output-stream "data")
		   (write-uint file-output-stream data-size 4))))
	(list
	 :open-file (lambda()
		      (open-file)
		      ;; Write preliminary chunks
		      (write-riff-chunk 0)
		      (write-format-chunk 0)
		      (write-data-chunk 0))
	 :write-sample
	 (lambda (sample)
	   "sample: -1.0 ... 1.0"
	   (setf sample-count (+ 1 sample-count))
	   (write-sample sample))
	 :close-file
	 (lambda ()
	   (if (< 0 sample-count)
	       (progn
		 ;; Update chunks
		 (file-position file-output-stream :start)
		 (write-riff-chunk sample-count)
		 (write-format-chunk sample-count)
		 (write-data-chunk sample-count)))
	   (close-file)))))))


(defun input-to-wave (f v-peak)
  ;; convert to -1.0 ... +1.0
  (/ f v-peak))

(defun make-module (name environment &key channel-count filename (v-peak 5.0) (sample-width :16bit))
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
    <li>:sample-width Resolution of samples. One of :8Bit, :16Bit, :24Bit</li> 
  </ul>
  The module has the following inputs:
  <ul>
      <li>:channel-1 ... :channel-n The sample values of the generated frames
	  are written in order :channel-1 ... :channel-n</li>
  </ul>
  The module has no outputs.
  <p>See also cl-synthesizer-monitor:add-monitor which provides Wave-File-Writing
     without having to add the module and the required patches to the rack.</p>"
  (if (<= channel-count 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: channel-count must be greater than 0: ~a"
       :format-arguments (list name channel-count)))
  (let ((inputs (cl-synthesizer-macro-util:make-keyword-list "channel" channel-count))
	(opened-wave-writer nil)
	(wave-writer (make-writer
		      :filename (merge-pathnames filename (getf environment :home-directory))
		      :channel-count channel-count
		      :sample-width sample-width
		      :sample-rate (floor (getf environment :sample-rate)))))
    ;; inputs are now (:CHANNEL-1 ... :CHANNEL-n)
    (list
     :inputs (lambda () inputs)
     :outputs (lambda () '())
     :get-output (lambda (output) (declare (ignore output)) nil)
     :update (lambda (args)
	       (if (not opened-wave-writer)
		   (progn
		     (setf opened-wave-writer t)
		     (funcall (getf wave-writer :open-file))))
	       (dolist (input inputs)
		 (let ((value (getf args input)))
		   (if (not value)
		       (setf value 0.0))
		   (funcall (getf wave-writer :write-sample) (input-to-wave value v-peak)))))
     :shutdown (lambda ()
		 (funcall (getf wave-writer :close-file))))))
