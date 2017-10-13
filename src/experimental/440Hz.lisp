;;
;; Generates a 440Hz cosinus wave
;;

(defpackage :cl-440Hz
  (:use :cl))

(in-package :cl-440Hz)

(defparameter *sample-rate* 44100)


(defun float-to-int16 (value)
  (if (> value 1.0)
      (break))
  (if (< value -1.0)
      (break))
  (let ((i (round (* 32000 value))))
    i))
  
(defun get-cosinus-440-hz-generator ()
  (let ((curPhi 0) (deltaPhi (/ (* 2 PI 440) *sample-rate*)))
    (lambda ()
      (let ((c (cos curPhi)))
	(setf curPhi (+ curPhi deltaPhi))
	c))))

(defun write-sound ()
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((wave (cl-wave:open-wave "/Users/olli/sample.wav" :direction :output)))
    (let ((frames nil) (generator (get-cosinus-440-hz-generator)))
      (dotimes (i 100000)
	(let ((v (funcall generator)))
	  (push (float-to-int16 v) frames)))
      (cl-wave:set-frames wave (nreverse frames)))
    (cl-wave:close-wave wave))
    "Done")


(write-sound)



