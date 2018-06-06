(in-package :cl-synthesizer-test)

(defparameter *control-table-cc*
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

(defparameter *CC-TEST-VENDOR*
    (list
     :get-controller-number
     (lambda (id)
       (let ((encoder-list (getf *control-table-cc* :ENCODER-CONTROLLER-NUMBERS)))
	 (let ((encoder (getf encoder-list id)))
	   (let ((controller-number (getf encoder :CONTROLLER-NUMBER)))
	     (if (not controller-number)
		 (format t "Controller not found: ~a" id))
	     controller-number))))
     :get-controller-value-offset
     (lambda (controller-value)
       (funcall (getf *control-table-cc* :RELATIVE-ENCODER-OFFSET) controller-value))))


(define-test test-clip-127 ()
	     (assert-equal 0 (cl-synthesizer-midi::clip-127 0))
	     (assert-equal 0 (cl-synthesizer-midi::clip-127 -1))
	     (assert-equal 127 (cl-synthesizer-midi::clip-127 127))
	     (assert-equal 127 (cl-synthesizer-midi::clip-127 128))
	     (assert-equal 5 (cl-synthesizer-midi::clip-127 5)))

(defparameter *CC-HANDLER-INITIAL-CV* 50)

(defmacro with-cc-handler (&body body)
  `(let* ((vendor *CC-TEST-VENDOR*)
	   (handler (funcall #' cl-synthesizer-midi:7-bit-relative
				vendor
				:ENCODER-2
				:cv-initial *CC-HANDLER-INITIAL-CV*
				:cv-min 0
				:cv-max 1270)))
     (let ((controller-number (funcall (getf vendor :get-controller-number) :ENCODER-2))
	   (update-fn (getf handler :update))
	    (output-fn (getf handler :get-output)))
	,@body)))

(define-test test-cc-1 ()
	     (with-cc-handler
	       (assert-equal *CC-HANDLER-INITIAL-CV* (funcall output-fn))))

(define-test test-cc-2 ()
	     (with-cc-handler
	       (funcall update-fn nil)
	       (assert-equal *CC-HANDLER-INITIAL-CV* (funcall output-fn))))

(define-test test-cc-3 ()
	     (with-cc-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 0)))
	       (assert-equal *CC-HANDLER-INITIAL-CV* (funcall output-fn))))

(define-test test-cc-inc-by-one ()
	     (with-cc-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 65)))
	       (assert-equal (+ *CC-HANDLER-INITIAL-CV* 10) (funcall output-fn))))

(define-test test-cc-inc-by-three ()
	     (with-cc-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 66)))
	       (assert-equal (+ *CC-HANDLER-INITIAL-CV* 30) (funcall output-fn))))

(define-test test-cc-inc-by-five ()
	     (with-cc-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)))
	       (assert-equal (+ *CC-HANDLER-INITIAL-CV* 50) (funcall output-fn))))

(define-test test-cc-inc-clip-top ()
	     (with-cc-handler
	       (dotimes (i 100)
		 (funcall update-fn
			  (list
			   (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67))))
	       (assert-equal 1270 (funcall output-fn))))

(define-test test-cc-dec-by-one ()
	     (with-cc-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 63)))
	       (assert-equal (- *CC-HANDLER-INITIAL-CV* 10) (funcall output-fn))))

(define-test test-cc-dec-by-three ()
	     (with-cc-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 62)))
	       (assert-equal (- *CC-HANDLER-INITIAL-CV* 30) (funcall output-fn))))

(define-test test-cc-dec-by-five ()
	     (with-cc-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)))
	       (assert-equal (- *CC-HANDLER-INITIAL-CV* 50) (funcall output-fn))))

(define-test test-cc-inc-clip-bottom ()
	     (with-cc-handler
	       (dotimes (i 100)
		 (funcall update-fn
			  (list
			   (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61))))
	       (assert-equal 0 (funcall output-fn))))

(define-test test-cc-update-multiple-events-1 ()
	     (with-cc-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 65)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 63)))
	       (assert-equal *CC-HANDLER-INITIAL-CV* (funcall output-fn))))

(define-test test-cc-update-multiple-events-2 ()
	     (with-cc-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 65)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 66)))
	       (assert-equal (+ *CC-HANDLER-INITIAL-CV* 40) (funcall output-fn))))

(define-test test-cc-update-multiple-events-clip-top ()
	     (with-cc-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 67)))
	       (assert-equal 1270 (funcall output-fn))))

(define-test test-cc-update-multiple-events-clip-bottom ()
	     (with-cc-handler
	       (funcall update-fn
			(list
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)
			 (cl-synthesizer-midi-event:make-control-change-event 1 controller-number 61)))
	       (assert-equal 0 (funcall output-fn))))

