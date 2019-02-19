(defpackage :cl-synthesizer-playground-write-wave
  (:use :cl))

(in-package :cl-synthesizer-playground-write-wave)


(defun write-stereo-samples (writer value)
  (dotimes (i 44100)
    (funcall (getf writer :write-sample) value)
    (funcall (getf writer :write-sample) value)))

(defun write-test-files ()

  (let ((writer (cl-synthesizer-modules-wave-file-writer::make-writer
	       :filename "/Users/olli/src/lisp/cl-synthesizer/playground/test8Bit.wav"
	       :channel-count 2
	       :sample-rate 44100
	       :sample-width :8Bit)))
    (funcall (getf writer :open-file))
    (funcall (getf writer :open-file))
    (write-stereo-samples writer 1.0)
    (write-stereo-samples writer -1.0)
    ;;(write-stereo-samples writer -1.0)
    (funcall (getf writer :close-file)))
  
  (let ((writer (cl-synthesizer-modules-wave-file-writer::make-writer
	       :filename "/Users/olli/src/lisp/cl-synthesizer/playground/test16Bit.wav"
	       :channel-count 2
	       :sample-rate 44100
	       :sample-width :16Bit)))
    (funcall (getf writer :open-file))
    (write-stereo-samples writer 1.0)
    (write-stereo-samples writer -1.0)
    ;; (write-stereo-samples writer -1.0)
    (funcall (getf writer :close-file)))


  "DONE")
  
;;(write-test-files)



