(in-package :cl-synthesizer-vendor-arturia-minilab-mk2)

(defparameter *CONTROL-TABLE*
  (list
   :ENCODER-CONTROL-NUMBERS
   (list :ENCODER-1 (list :CONTROL-NUMBER 112)
	 :ENCODER-2 (list :CONTROL-NUMBER 74)
	 :ENCODER-3 (list :CONTROL-NUMBER 71)
	 :ENCODER-4 (list :CONTROL-NUMBER 76)
	 :ENCODER-5 (list :CONTROL-NUMBER 77)
	 :ENCODER-6 (list :CONTROL-NUMBER 93)
	 :ENCODER-7 (list :CONTROL-NUMBER 73)
	 :ENCODER-8 (list :CONTROL-NUMBER 75)
	 :ENCODER-9 (list :CONTROL-NUMBER 114)
	 :ENCODER-10 (list :CONTROL-NUMBER 18)
	 :ENCODER-11 (list :CONTROL-NUMBER 19)
	 :ENCODER-12 (list :CONTROL-NUMBER 16)
	 :ENCODER-13 (list :CONTROL-NUMBER 17)
	 :ENCODER-14 (list :CONTROL-NUMBER 91)
	 :ENCODER-15 (list :CONTROL-NUMBER 79)
	 :ENCODER-16 (list :CONTROL-NUMBER 72))
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

