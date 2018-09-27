(in-package :cl-synthesizer-midi)

;;
;;
;; midi note to frequency mapping
;; Midi note 69 -> 440Hz
;;

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
  "Returns the frequency of a given note number. Note number 69 results in a frequency of 440Hz.
   This function implements a simple mapping and might be useful in some cases. For more
   details about the implementation refer to the source code."
  (elt *TUNING-TABLE* note-number))

;; (get-note-number-frequency 69)
