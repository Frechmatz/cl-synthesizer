;;
;;
;; Play sinus wave using cl-out123 
;;
;;

(defpackage :out123-example
  (:use :cl :cl-out123))

(in-package :out123-example)

(defun sinus-generator (frequency sample-rate)
  (let ((curPhi 0) (current-output 1.0))
    (lambda ()
      (let ((deltaPhi (/ (* 2 PI frequency) sample-rate)))
	(setf current-output (sin curPhi))
	(setf curPhi (+ curPhi deltaPhi))
	current-output))))

(defun make-sinus-buffer (duration-seconds sample-rate)
  (let ((gain 1)
	(buf (make-array (* duration-seconds sample-rate) :element-type 'single-float :adjustable nil))
	(pos 0)
	(sinus (sinus-generator 440 44100)))
      (dotimes (counter (* duration-seconds sample-rate))
	(let ((f (funcall sinus)))
	  (let ((f-amplified (* gain f)))
	    (setf (aref buf pos) (coerce f-amplified 'single-float))
	    (setf pos (+ pos 1)))))
    buf))

(defun play-sinus (driver)
  (let ((out (make-instance 'cl-out123:output)))
    (let ((drivers (cl-out123:drivers out)))
      (format t "~%Drivers: ~a~%" drivers)
      (cl-out123:connect out :driver driver)
      (let ((formats (cl-out123:formats out '(44100) 1 2)))
	(format t "~%Formats: ~a~%" formats)
	(let ((buf (make-sinus-buffer 2 44100)))
	  (cl-out123:start out :rate 44100 :channels 1 :encoding :float)
	  (cl-out123:play out buf))))
    (cl-out123:disconnect out)
    "DONE"))

;; (play-sinus "coreaudio")
