(in-package :cl-synthesizer-vendor-arturia-minilab-mk2)

(defparameter *CONTROL-TABLE*
  (list
   :ENCODER-CONTROLLER-NUMBERS
   (list :ENCODER-1 (list :CONTROLLER-NUMBER 112)
	 :ENCODER-2 (list :CONTROLLER-NUMBER 74)
	 :ENCODER-3 (list :CONTROLLER-NUMBER 71)
	 :ENCODER-4 (list :CONTROLLER-NUMBER 76)
	 :ENCODER-5 (list :CONTROLLER-NUMBER 77)
	 :ENCODER-6 (list :CONTROLLER-NUMBER 93)
	 :ENCODER-7 (list :CONTROLLER-NUMBER 73)
	 :ENCODER-8 (list :CONTROLLER-NUMBER 75)
	 :ENCODER-9 (list :CONTROLLER-NUMBER 114)
	 :ENCODER-10 (list :CONTROLLER-NUMBER 18)
	 :ENCODER-11 (list :CONTROLLER-NUMBER 19)
	 :ENCODER-12 (list :CONTROLLER-NUMBER 16)
	 :ENCODER-13 (list :CONTROLLER-NUMBER 17)
	 :ENCODER-14 (list :CONTROLLER-NUMBER 91)
	 :ENCODER-15 (list :CONTROLLER-NUMBER 79)
	 :ENCODER-16 (list :CONTROLLER-NUMBER 72))
   :RELATIVE-ENCODER-OFFSET
   (lambda (controller-value)
     (cond
       ((eq 61 controller-value) -5)
       ((eq 62 controller-value) -3)
       ((eq 63 controller-value) -1)
       ((eq 65 controller-value) 1)
       ((eq 66 controller-value) 3)
       ((eq 67 controller-value) 5)
       (t 0)))))

(defun get-device-settings ()
  *CONTROL-TABLE*)
