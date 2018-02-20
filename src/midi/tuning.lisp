(in-package :cl-synthesizer-midi)

(defparameter *TUNING-TABLE*
  ;; http://subsynth.sourceforge.net/midinote2freq.html
  (let ((arr (make-array 128))
	(a 440))
    (dotimes (x (length arr))
      (let ((frequency
	     (* (/ a 32) (expt 2 (/ (+ x -9) 12)))))
	(setf (aref arr x) frequency)))
    arr))

(defun get-note-number-frequency (note-number)
  (elt *TUNING-TABLE* note-number))

;; (get-note-number-frequency 69)
