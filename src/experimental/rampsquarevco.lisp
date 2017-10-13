;;
;; Cosinus VCO with Ramp and Square CV
;;

(defpackage :cl-rampsquarevco
  (:use :cl))

(in-package :cl-rampsquarevco)

(defparameter *sample-rate* 44100)


(defun float-to-int16 (value)
  (if (> value 1.0)
      (break))
  (if (< value -1.0)
      (break))
  (let ((i (round (* 32000 value))))
    i))
  
(defun get-cosinus-vco-generator (v_in_generator &key (f_0 2) (f_delta 50))
  (declare (optimize (debug 3) (speed 0) (space 0)))
  ;; if f_0 < f_delta
  (if (>= f_delta f_0)
      (error "f_0 must be greater than f_delta"))
  (let* ((curPhi 0))
    (flet ((get-cur-frequency (v_in)
	     (declare (optimize (debug 3) (speed 0) (space 0)))
	     (+ f_0 (* v_in f_delta)))
	   (get-delta-phi (cur-frequency)
	     (let ((deltaPhi (/ (* 2 PI cur-frequency) *sample-rate*)))
	       deltaPhi)))
      (lambda ()
	(declare (optimize (debug 3) (speed 0) (space 0)))
	(let ((v_in (funcall v_in_generator)))
	  (let ((deltaPhi (get-delta-phi (get-cur-frequency v_in))))
	    (let ((c (cos curPhi)))
	      (setf curPhi (+ curPhi deltaPhi))
	      (values c v_in))))))))

(defun get-ramp-up-generator (start max seconds)
  (let ((cur-value start) (delta (/ max (* seconds *sample-rate*))))
    (lambda ()
      (if (>= cur-value max)
	  max
	  (let ((r cur-value))
	    (setf cur-value (+ cur-value delta))
	    r)))))

(defun get-square-wave-generator (frequency)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((cur-tick 0) (out 1.0))
    (flet ((get-ticks (frequency)
	     (* (/ *sample-rate* frequency 2) )))
      (lambda ()
	(declare (optimize (debug 3) (speed 0) (space 0)))
	(let ((ticks (get-ticks frequency)))
	  (let ((r out))
	    (setf cur-tick (+ cur-tick 1))
	    (if (< ticks cur-tick)
		(progn
		  (setf cur-tick 0)
		  (setf out (* out -1 ))))
	    (values r 0)))))))


(defun write-sound (filename generator)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((wave (cl-wave:open-wave filename :direction :output)))
    (cl-wave:set-num-channels wave 2)
    (let ((frames nil))
      (dotimes (i 100000)
	(multiple-value-bind (v i) (funcall generator)
	  (push (float-to-int16 v) frames)
	  (push (float-to-int16 i) frames)))
      (cl-wave:set-frames wave (nreverse frames)))
    (cl-wave:close-wave wave))
    "Done")

(defun write-sound-square ()
  (write-sound "/Users/olli/square.wav"
	       (get-cosinus-vco-generator
		(get-square-wave-generator 2)
		:f_0 100 :f_delta 75)))

;; (write-sound-square)

(defun write-sound-ramp ()
  (write-sound "/Users/olli/ramp.wav"
	       (get-cosinus-vco-generator
		(get-ramp-up-generator -1.0 1.0 1)
		:f_0 20 :f_delta 19)))

;; (write-sound-ramp)

