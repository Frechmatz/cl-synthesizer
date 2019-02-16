(defpackage :cl-synthesizer-playground-write-wave
  (:use :cl))

(in-package :cl-synthesizer-playground-write-wave)

(defun write-test-files ()
  (let ((writer (cl-synthesizer-modules-wave-file-writer::make-writer
	       :filename "/Users/olli/cl-synthesizer-examples/test16Bit.wav"
	       :channel-count 2
	       :sample-rate 44100
	       :sample-width :16Bit)))
    (funcall (getf writer :open-file))
    (dotimes (i 44100)
      (funcall (getf writer :write-sample) 0.9)
      (funcall (getf writer :write-sample) 0.9)
      )
    (dotimes (i 44100)
      (funcall (getf writer :write-sample) -0.9)
      (funcall (getf writer :write-sample) -0.9)
      )
    (dotimes (i 44100)
      (funcall (getf writer :write-sample) 0.9)
      (funcall (getf writer :write-sample) 0.9)
      )
    (funcall (getf writer :close-file)))

  (let ((writer (cl-synthesizer-modules-wave-file-writer::make-writer
	       :filename "/Users/olli/cl-synthesizer-examples/test8Bit.wav"
	       :channel-count 2
	       :sample-rate 44100
	       :sample-width :8Bit)))
    (funcall (getf writer :open-file))
    (dotimes (i 44100)
      (funcall (getf writer :write-sample) 0.9)
      (funcall (getf writer :write-sample) 0.9)
      )
    (dotimes (i 44100)
      (funcall (getf writer :write-sample) -0.9)
      (funcall (getf writer :write-sample) -0.9)
      )
    (dotimes (i 44100)
      (funcall (getf writer :write-sample) 0.9)
      (funcall (getf writer :write-sample) 0.9)
      )
    (funcall (getf writer :close-file)))

  "DONE")
  
;;(write-test-files)



