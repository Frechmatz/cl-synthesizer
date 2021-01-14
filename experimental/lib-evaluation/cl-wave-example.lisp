;;
;;
;; Read and write Wave file using cl-wave library
;;
;;

(defpackage :cl-wave-example
  (:use :cl))

(in-package :cl-wave-example)

(ql:quickload "cl-wave")


(defun read-wave ()
  (let ((wave (cl-wave:open-wave
	       (merge-pathnames
		"src/lisp/cl-synthesizer/lib-evaluation/example.wav"
		(user-homedir-pathname))
	       :direction :input)))
    (format t "~%Sample-Rate: ~a~%" (cl-wave:get-sample-rate wave))
    (cl-wave:close-wave wave)
    wave))

;;(read-wave)

(defun write-wave ()
  ;; Read reference file and clone it
  (let ((input-wave (read-wave)))
    (let ((wave (cl-wave:open-wave
		 (merge-pathnames
		  "example-clone.wav"
		  (user-homedir-pathname))
		 :direction :output)))
      (cl-wave:set-num-channels wave (cl-wave:get-num-channels input-wave))
      (cl-wave:set-sample-rate wave (cl-wave:get-sample-rate input-wave))
      ;; Clone frames
      (let ((samples nil))
	(dolist (sample (cl-wave:get-frames input-wave))
	  (push sample samples))
	(cl-wave:set-frames wave (reverse samples))
	(cl-wave:close-wave wave))))
  (format t "~%Cloned file~%")
  "DONE")

;;(write-wave)



