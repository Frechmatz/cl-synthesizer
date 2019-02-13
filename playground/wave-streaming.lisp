;;
;; Streaming wave file writer
;;

(defpackage :cl-synthesizer-playground-wave-streaming
  (:use :cl))

(in-package :cl-synthesizer-playground-wave-streaming)


(defun write-byte-into-stream (b stream)
  (write-byte b stream))


;; http://www.asciitable.com/
(defparameter *RIFF-BYTE* 82)
(defparameter *FORMAT-BYTE* 70)
(defparameter *DATA-BYTE* 68)

(defun make-writer (&key filename channel-count sample-rate)
  (declare (ignore channel-count sample-rate))
  (let ((sample-count 0) (file-output-stream))
    (flet ((write-samples (samples count)
	     ;; TODO Write bytes
	     (dotimes (i count)
	       (write-byte-into-stream (nth i samples) file-output-stream)))
	   (open-file ()
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
	   (seek-to-start ()
	     (file-position file-output-stream :start))
	   (write-riff-chunk (number-of-samples)
	     ;; TODO Write chunk
	     (write-byte-into-stream *RIFF-BYTE* file-output-stream)
	     (if (= 0 number-of-samples)
		 (write-byte-into-stream 0 file-output-stream)
		 (write-byte-into-stream 1 file-output-stream))
	     nil)
	   (write-format-chunk ()
	     ;; TODO Write chunk
	     (write-byte-into-stream *FORMAT-BYTE* file-output-stream)
	     (write-byte-into-stream 0 file-output-stream)
	     nil)
	   (write-data-chunk (number-of-samples)
	     (write-byte-into-stream *DATA-BYTE* file-output-stream)
	     (if (= 0 number-of-samples)
		 (write-byte-into-stream 0 file-output-stream)
		 (write-byte-into-stream 1 file-output-stream))
	     ;; TODO Write chunk
	     nil)
	   )
      (list
       :open-file
       (lambda()
	 (open-file)
	 (write-riff-chunk 0) ;; Write preliminary RIFF chunk
	 (write-format-chunk) ;; Write FMT chunk
	 (write-data-chunk 0)) ;; Write preliminary DATA chunk
       :write-samples
       (lambda (samples &key count)
	 (setf sample-count (+ sample-count count))
	 (write-samples samples count))
       :close-file
       (lambda ()
	 (if (< 0 sample-count)
	     (progn
	       (seek-to-start)
	       (write-riff-chunk sample-count)
	       (write-format-chunk)
	       (write-data-chunk sample-count)))
	 (close-file))
       )
      )))

(defun lets-go-simple ()
  (let ((w (make-writer :filename "/Users/olli/cl-synthesizer-examples/stream.bin" :channel-count 2 :sample-rate 44100)))
    (funcall (getf w :open-file))
    (funcall (getf w :close-file))
  ))

;; (lets-go-simple)

(defun lets-less-simple ()
  (let ((w (make-writer :filename "/Users/olli/cl-synthesizer-examples/stream.bin" :channel-count 2 :sample-rate 44100)))
    (funcall (getf w :open-file))
    (funcall (getf w :write-samples) (list 128 129 130 131 132 133) :count 6)
    (funcall (getf w :close-file))
  ))

;; (lets-less-simple)






