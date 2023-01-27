(in-package :cl-synthesizer-modules-wave-file-writer)

(defun make-symbol-impl (name num package)
  (if num
      (intern (format nil "~a-~a" (string-upcase name) num) package)
      (intern (string-upcase name) package)))

(defun make-keyword (name num)
  (make-symbol-impl name num "KEYWORD"))

(defun make-keyword-list (name count)
  "Returns list of keywords ordered by number of keyword: (:<name>-1, :<name>-2, ..., <name>-<count>.
   The numbering starts by one."
  (let ((l nil))
    (dotimes (i count)
      (push (make-keyword name (+ i 1)) l))
    (nreverse l)))

;;
;; Factory function to be overridden by tests
;;
(defvar *make-writer* (lambda (&rest args)
			(apply #'cl-wave-file-writer:make-writer args)))

(defun input-to-wave (f v-peak)
  ;; convert to -1.0 ... +1.0
  (* (/ (min (abs f) v-peak) v-peak) (signum f)))  

(defun make-module (name environment &key channel-count filename v-peak (sample-width :16bit))
  "Creates a Wave File Writer module. Writes files in \"Waveform Audio File\" (\"WAV\") format.
    <p>The function has the following parameters:
  <ul>
    <li>name Name of the writer.</li>
    <li>environment The synthesizer environment.</li>
    <li>:channel-count Number of channels.</li>
    <li>:filename The relative path of the file to be written. The filename will be concatenated
        with the base path as defined by the :home-directory property of the environment.</li>
    <li>:v-peak Absolute value of peak voltage.</li>
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
      (error
       'cl-synthesizer:assembly-error
       :format-control "'~a': channel-count must be greater than 0: '~a'"
       :format-arguments (list name channel-count)))
  (if (not v-peak)
      (error
       'cl-synthesizer:assembly-error
       :format-control "'~a': v-peak must not be nil"
       :format-arguments (list name)))
  (if (> 0 v-peak)
      (error
       'cl-synthesizer:assembly-error
       :format-control "'~a': v-peak must not be negative: '~a'"
       :format-arguments (list name v-peak)))
  (let ((input-sockets (make-keyword-list "channel" channel-count))
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
