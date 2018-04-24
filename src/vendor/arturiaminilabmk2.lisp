(in-package :cl-synthesizer-vendor-arturia-minilab-mk2)

(defparameter *CONTROL-TABLE*
  '(:ENCODER-1 (:CONTROL-NUMBER 112)
    :ENCODER-2 (:CONTROL-NUMBER 74)
    :ENCODER-3 (:CONTROL-NUMBER 71)
    :ENCODER-4 (:CONTROL-NUMBER 76)
    :ENCODER-5 (:CONTROL-NUMBER 77)
    :ENCODER-6 (:CONTROL-NUMBER 93)
    :ENCODER-7 (:CONTROL-NUMBER 73)
    :ENCODER-8 (:CONTROL-NUMBER 75)
    :ENCODER-9 (:CONTROL-NUMBER 114)
    :ENCODER-10 (:CONTROL-NUMBER 18)
    :ENCODER-11 (:CONTROL-NUMBER 19)
    :ENCODER-12 (:CONTROL-NUMBER 16)
    :ENCODER-13 (:CONTROL-NUMBER 17)
    :ENCODER-14 (:CONTROL-NUMBER 91)
    :ENCODER-15 (:CONTROL-NUMBER 79)
    :ENCODER-16 (:CONTROL-NUMBER 72)
    ))

(defun get-control-number (id)
  (getf (getf *CONTROL-TABLE* id) :CONTROL-NUMBER))

(defun get-cc-offset-relative-1 (controller-value)
  (cond
    ((eq 61 controller-value) -5)
    ((eq 62 controller-value) -3)
    ((eq 63 controller-value) -1)
    ((eq 65 controller-value) 1)
    ((eq 66 controller-value) 3)
    ((eq 67 controller-value) 5)
    (t 0)))

(defun clip-7-bit (controller-state)
  (cond
    ((> controller-state 127)
     127)
    ((< 0 controller-state)
     0)
    (t controller-state)))

(defun cc-7-bit-relative-1 (controller-number &key (cv-initial 2.5) (cv-min 0) (cv-max 5))
  (let* ((converter (cl-synthesizer-core:linear-converter
		     :input-min 0
		     :input-max 127
		     :output-min cv-min
		     :output-max cv-max))
	 (controller-state (funcall (getf converter :get-x) cv-initial)))
    (lambda (midi-events)
      (dolist (midi-event midi-events)
	(if (and midi-event
		 (cl-synthesizer-midi-event:control-change-eventp midi-event)
		 (eq controller-number (cl-synthesizer-midi-event:get-control-number midi-event)))
	    (setf controller-state
		  (+ (get-cc-offset-relative-1
		      (cl-synthesizer-midi-event:get-control-value midi-event))))))
      (setf controller-state (clip-7-bit controller-state))
      (funcall (getf converter :get-y) controller-state))))

 
