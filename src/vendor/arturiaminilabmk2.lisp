(in-package :cl-synthesizer-vendor)

(defparameter *arturia-minilab-mk2*
  (list
   :get-controller-number
   (lambda (id)
     (getf 
      '(:ENCODER-1 112
	:ENCODER-2 74
	:ENCODER-3 71
	:ENCODER-4 76
	:ENCODER-5 77
	:ENCODER-6 93
	:ENCODER-7 73
	:ENCODER-8 75
	:ENCODER-9 114
	:ENCODER-10 18
	:ENCODER-11 19
	:ENCODER-12 16
	:ENCODER-13 17
	:ENCODER-14 91
	:ENCODER-15 79
	:ENCODER-16 72)
      id))
   :get-controller-value-offset
   (lambda (controller-value)
     (cond
       ((eq 61 controller-value) -5)
       ((eq 62 controller-value) -3)
       ((eq 63 controller-value) -1)
       ((eq 65 controller-value) 1)
       ((eq 66 controller-value) 3)
       ((eq 67 controller-value) 5)
       (t 0)))))
