(in-package :cl-synthesizer-test)

(defparameter *control-table-cc-14*
  (list
   :ENCODER-CONTROLLER-NUMBERS
   (list :ENCODER-1 (list :CONTROLLER-NUMBER 112)
	 :ENCODER-2 (list :CONTROLLER-NUMBER 74))
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

(defparameter *CC-14-TEST-VENDOR*
  (list
   :get-controller-number
   (lambda (id)
     (let ((encoder-list (getf *control-table-cc-14* :ENCODER-CONTROLLER-NUMBERS)))
       (let ((encoder (getf encoder-list id)))
	   (let ((controller-number (getf encoder :CONTROLLER-NUMBER)))
	     (if (not controller-number)
		 (format t "Controller not found: ~a" id))
	     controller-number))))
   :get-controller-value-offset
   (lambda (controller-value)
     (funcall (getf *control-table-cc-14* :RELATIVE-ENCODER-OFFSET) controller-value))))

  

(defparameter *CC-14-HANDLER-INITIAL-CV* 50)

(defparameter *LSB-CONTROLLER* :ENCODER-2)
(defparameter *MSB-CONTROLLER* :ENCODER-1)

(defmacro with-cc-14-handler (&body body)
  `(let* ((vendor *CC-14-TEST-VENDOR*)
	   (handler (funcall #' cl-synthesizer-midi:14-bit-relative
				vendor
				:controller-id-msb *MSB-CONTROLLER*
				:controller-id-lsb *LSB-CONTROLLER*
				:cv-initial *CC-14-HANDLER-INITIAL-CV*
				:cv-min 0
				:cv-max 16383)))
     (let ((controller-number-lsb (funcall (getf vendor :get-controller-number) *LSB-CONTROLLER*))
	   (controller-number-msb (funcall (getf vendor :get-controller-number) *MSB-CONTROLLER*))
	   (update-fn (getf handler :update))
	    (output-fn (getf handler :get-output)))
	,@body)))

(define-test test-cc-14-1 ()
	     (with-cc-14-handler
	       (assert-equal *CC-14-HANDLER-INITIAL-CV* (funcall output-fn))))

(define-test test-cc-14-2 ()
	     (with-cc-14-handler
	       (funcall update-fn nil)
	       (assert-equal *CC-14-HANDLER-INITIAL-CV* (funcall output-fn))))


(define-test test-cc-14-3 ()
	     (with-cc-14-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-lsb 0)))
	       (assert-equal *CC-14-HANDLER-INITIAL-CV* (funcall output-fn))))


(define-test test-cc-14-inc-by-one ()
	     (with-cc-14-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-lsb 65)))
	       (assert-equal (+ *CC-14-HANDLER-INITIAL-CV* 1) (funcall output-fn))))

(define-test test-cc-14-inc-by-three ()
	     (with-cc-14-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-lsb 66)))
	       (assert-equal (+ *CC-14-HANDLER-INITIAL-CV* 3) (funcall output-fn))))


(define-test test-cc-14-inc-by-five ()
	     (with-cc-14-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-lsb 67)))
	       (assert-equal (+ *CC-14-HANDLER-INITIAL-CV* 5) (funcall output-fn))))


(define-test test-cc-14-inc-clip-top ()
	     (with-cc-14-handler
	       (dotimes (i 100000)
		 (funcall update-fn
			  (list
			   (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-lsb 67))))
	       (assert-equal 16383 (funcall output-fn))))

(define-test test-cc-14-dec-by-one ()
	     (with-cc-14-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-lsb 63)))
	       (assert-equal (- *CC-14-HANDLER-INITIAL-CV* 1) (funcall output-fn))))


(define-test test-cc-14-dec-by-three ()
	     (with-cc-14-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-lsb 62)))
	       (assert-equal (- *CC-14-HANDLER-INITIAL-CV* 3) (funcall output-fn))))


(define-test test-cc-14-dec-by-five ()
	     (with-cc-14-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-lsb 61)))
	       (assert-equal (- *CC-14-HANDLER-INITIAL-CV* 5) (funcall output-fn))))

(define-test test-cc-14-inc-clip-bottom ()
	     (with-cc-14-handler
	       (dotimes (i 100)
		 (funcall update-fn
			  (list
			   (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-lsb 61))))
	       (assert-equal 0 (funcall output-fn))))

(define-test test-cc-14-update-multiple-events-1 ()
	     (with-cc-14-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-lsb 65)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-lsb 63)))
	       (assert-equal *CC-14-HANDLER-INITIAL-CV* (funcall output-fn))))

(define-test test-cc-14-update-multiple-events-2 ()
	     (with-cc-14-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-lsb 65)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-lsb 66)))
	       (assert-equal (+ *CC-14-HANDLER-INITIAL-CV* 4) (funcall output-fn))))

(define-test test-cc-14-inc-by-128 ()
	     (with-cc-14-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 65)))
	       (assert-equal (+ *CC-14-HANDLER-INITIAL-CV* 128) (funcall output-fn))))


(define-test test-cc-14-update-multiple-events-clip-top ()
	     (with-cc-14-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 67)))
	       (assert-equal 16383 (funcall output-fn))))

(define-test test-cc-14-update-multiple-events-clip-bottom ()
	     (with-cc-14-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number-msb 61)))
	       (assert-equal 0 (funcall output-fn))))

