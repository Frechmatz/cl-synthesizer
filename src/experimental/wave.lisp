

(in-package :cl-wave)

(defclass cl-wave-mono-16-bit ()
  (
   (samples :initform nil)
   ))

(defgeneric add-sample (cl-wave-mono-16-bit sample))
(defgeneric write-wave (cl-wave-mono-16-bit filename))

(defmethod add-sample ((wav cl-wave-mono-16-bit) sample)
  (push (slot-value wav 'samples) sample))

(defmethod write-wave ((wav cl-wave-mono-16-bit) filename)
  (with-open-file (f filename :direction :output :if-does-not-exist :create :if-exists :overwrite :element-type '(unsigned-byte 8))
    (let ((os (flexi-streams:make-flexi-stream f :external-format :us-ascii)))
      (write-char #\R os)
      (write-char #\I os)
      (write-char #\F os)
      (write-char #\F os)
      (finish-output os)
    )))


(defun test ()
  (let ((wav (make-instance 'cl-wave-mono-16-bit)))
    (dotimes (i 64000)
      (add-sample wav i))
    (write-wave wav "/Users/olli/sample.wav")))

;;(test)




(defparameter *olli* (list :chunk-id 1000 :sample-rate 44000))
(getf *olli* :sample-rate)




	    
